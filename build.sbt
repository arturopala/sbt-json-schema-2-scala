import uk.gov.hmrc.DefaultBuildSettings.targetJvm

val pluginName = "sbt-json-schema-2-scala"

lazy val root = Project(pluginName, base = file("."))
  .enablePlugins(SbtAutoBuildPlugin, SbtArtifactory)
  .settings(
    makePublicallyAvailableOnBintray := true
  )
  .settings(ScriptedPlugin.scriptedSettings)
  .settings(
    sbtPlugin := true,
    targetJvm := "jvm-1.7",
    organization := "uk.gov.hmrc",
    scalaVersion := "2.10.7",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.6.13",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      "org.pegdown" % "pegdown" % "1.6.0" % Test,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test
    ),
    scriptedLaunchOpts += ("-Dplugin.version=" + version.value),
    scriptedLaunchOpts ++= sys.process.javaVmArguments.filter(
      a => Seq("-Xmx", "-Xms", "-XX", "-Dsbt.log.noformat").exists(a.startsWith)
    ),
    scriptedBufferLog := false,
    scalafmtOnCompile in Compile := true,
    scalafmtOnCompile in Test := true
  )