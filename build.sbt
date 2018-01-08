

name := "cats-maps-sets"

version := "1.0.1"

lazy val catsVersion = "1.0.1"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.4")

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % catsVersion,
  "org.typelevel" %%% "cats-laws" % catsVersion,
  "org.typelevel" %%% "cats-testkit" % catsVersion % Test
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
