package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.EdgeKeysOdb
import io.shiftleft.codepropertygraph.generated.edges.ContainsNode
import io.shiftleft.codepropertygraph.generated.nodes.{CpgNode, NewNode, StoredNode}
import io.shiftleft.passes.DiffGraph
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.Property
import overflowdb.traversal.Traversal

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraph.Builder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasStoreMethod {
  import NewNodeSteps.logger

  override def store()(implicit diffBuilder: DiffGraph.Builder): Unit =
    traversal.sideEffect(storeRecursively).iterate

  private def storeRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraph.Builder): Unit = {
    diffBuilder.addNode(newNode)

    // add all `contained` nodes that are NewNodes to the DiffGraph
    newNode.allContainedNodes.collect {
      case containedNode: NewNode => storeRecursively(containedNode)
    }

    // create edges to `contained` nodes for this new node
    for {
      (localName, containedNodes) <- newNode.containedNodesByLocalName
      (containedNode, index) <- containedNodes.zipWithIndex
    } {
      val properties = Seq(
        EdgeKeysOdb.LOCAL_NAME -> localName,
        EdgeKeysOdb.INDEX -> index
      ).map { case Property(key, value) => (key.name, value) }
      addEdge(diffBuilder, newNode, containedNode, ContainsNode.Label, properties)
    }
  }

  private def addEdge(diffBuilder: DiffGraph.Builder,
                      src: CpgNode,
                      dst: CpgNode,
                      label: String,
                      properties: Seq[(String, AnyRef)]): Unit =
    (src, dst) match {
      case (src: NewNode, dst: NewNode) => diffBuilder.addEdge(src, dst, label, properties)
      case (src: NewNode, dst: StoredNode) =>
        diffBuilder.addEdgeToOriginal(src, dst, label, properties)
      case (src: StoredNode, dst: NewNode) =>
        diffBuilder.addEdgeFromOriginal(src, dst, label, properties)
      case (src: StoredNode, dst: StoredNode) =>
        diffBuilder.addEdgeInOriginal(src, dst, label, properties)
      case (src, dst) =>
        val srcClassMaybe = Option(src).map(_.getClass)
        val dstClassMaybe = Option(dst).map(_.getClass)
        logger.warn(
          s"unhandled case, likely produced by a fauly pass: src=$src, src.getClass=$srcClassMaybe, dst=$dst, dstClass=$dstClassMaybe")
    }

  def label: Steps[String] = new Steps(traversal.map(_.label))
}

object NewNodeSteps {
  private val logger: Logger = LoggerFactory.getLogger(classOf[NewNodeSteps[_]])
}
