
scalaVersion := "2.11.8"

lazy val algebra = (project in file(".")).
  settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

)
