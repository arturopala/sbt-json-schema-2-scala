sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("uk.gov.hmrc" % "sbt-json-schema-2-scala" % x)
  case _ => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}