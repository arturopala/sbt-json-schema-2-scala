package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.Schema

trait CodeGenerator {

  type CodeGeneratorOptions
  type CodeGeneratorResult = Either[List[String], Seq[Code]]

  def generateCodeFrom(schema: Schema, options: CodeGeneratorOptions, description: String): CodeGeneratorResult
}
