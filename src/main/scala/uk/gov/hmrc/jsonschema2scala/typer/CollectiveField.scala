package uk.gov.hmrc.jsonschema2scala.typer

import uk.gov.hmrc.jsonschema2scala.schema.Schema

case class CollectiveField(fieldName: String, schema: Schema, ids: Seq[String])
