package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.Schema

trait CodeRenderer {

  type CodeRendererOptions
  type CodeRenderingResult = Either[List[String], Seq[Code]]

  def render(schema: Schema, options: CodeRendererOptions, description: String): CodeRenderingResult
}
