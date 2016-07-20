
scalaVersion := "2.11.8"

lazy val algebra = (project in file(".")).
  settings(
    libraryDependencies ++= List(
        "org.scalatest" %% "scalatest" % "2.2.6" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
        "org.apache.commons" % "commons-math3" % "3.6.1"))
