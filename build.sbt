enablePlugins(ScalaJSPlugin)

name := "cats-maps-sets"

version := "1.0.0-RC1"

lazy val catsVersion = "1.0.0-RC1"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.4")

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % "1.0.0-RC1",
  "org.typelevel" %%% "cats-laws" % "1.0.0-RC1",
  "org.typelevel" %%% "cats-testkit" % "1.0.0-RC1" % Test
)
