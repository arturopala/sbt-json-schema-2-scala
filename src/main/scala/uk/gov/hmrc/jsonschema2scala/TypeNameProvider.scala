package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.Schema

trait TypeNameProvider {

  def toTypeName(schema: Schema): String

  def toTypeNameVariant(schema: Schema, pos: Int): String

  def toTypePatternName(schema: Schema): String

  def safe(name: String): String

}
