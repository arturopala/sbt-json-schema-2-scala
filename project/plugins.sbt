resolvers += Resolver.url("HMRC SBT Plugin Releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns)

resolvers += Resolver.bintrayRepo("hmrc", "releases")

resolvers += Resolver.url("ivy-releases", url("https://dl.bintray.com/typesafe/ivy-releases/"))(
  Resolver.ivyStylePatterns)

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "2.0.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-artifactory" % "1.0.0")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.16")
