libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "io.shiftleft" %% "overflowdb-codegen" % "1.20-SNAPSHOT",
)

resolvers += Resolver.bintrayRepo("shiftleft", "maven")
 // "1.20-SNAPSHOT", "1.17"