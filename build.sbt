scalaVersion := "3.5.0"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:fewerBraces",
  "-Xfatal-warnings"
)
