
scalaVersion := "2.11.8"

lazy val algebra = (project in file(".")).
  settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  )
