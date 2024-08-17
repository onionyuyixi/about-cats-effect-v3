

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "about-cats-effect-v3"
  ).settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "org.typelevel" %% "cats-free" % "2.12.0",
      "org.typelevel" %% "cats-mtl" % "1.4.0",
      "com.chuusai" %% "shapeless" % "2.3.12",
      "org.typelevel" %% "scalac-compat-annotation" % "0.1.4",
      "com.softwaremill.macwire" %% "macros" % "2.5.9" % Provided,
    )
  )
