import uk.gov.hmrc.jsonschema2scala.JsonSchema2ScalaFeature

lazy val root = (project in file("."))
  .enablePlugins(SbtJsonSchema2ScalaPlugin)
  .settings(
    organization := "uk.gov.hmrc",
    name := "simple",
    scalaVersion := "2.11.11",
    version := "0.1",
    jsonSchema2ScalaFeatures in Compile := Set(),
    jsonSchema2ScalaPackageName in Compile := "a.b.c",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.5.19",
      "org.typelevel" %% "cats-core" % "1.5.0",
      "uk.gov.hmrc" %% "stub-data-generator" % "0.5.3",
      "wolfendale" %% "scalacheck-gen-regexp" % "0.1.1"
    )
  )