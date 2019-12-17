package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.Schema

trait TypeNameProvider {

  def toTypeName(schema: Schema): String

  def safe(name: String): String

}
