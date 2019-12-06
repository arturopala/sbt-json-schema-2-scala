package uk.gov.hmrc.jsonschema2scala

import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.jsonschema2scala.JsonSchema.Schema

trait CodeRenderingAssertions extends CompilationAssertions {
  self: Assertions with Matchers =>

  def assertCanParseAndCompile(schema: String): Unit =
    assertCanParseAndCompile(schema, packageName = "a.b.c", className = "Test")

  def assertCanParseAndCompile(schema: JsonSchema.Schema): Unit =
    assertCanParseAndCompile(schema, Map())

  def assertCanParseAndCompile(schema: JsonSchema.Schema, references: Map[String, Schema]): Unit = {
    val definition = JsonSchema.read(schema.content, references)
    assertCanParseAndCompile(definition, packageName = "a.b.c", className = schema.className)
  }

  def assertCanParseAndCompile(schema: String, packageName: String, className: String): Unit = {
    val schemaJson = Json.parse(schema).as[JsObject]
    assertCanParseAndCompile(schemaJson, packageName, className)
  }

  def assertCanParseAndCompile(schemaJson: JsObject, packageName: String, className: String): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = packageName)
    val definition = JsonSchema.read(schemaJson)
    val code = JsonSchema2ScalaCodeRenderer.render(className, definition, options, "")
    assertCompiles(code, ClassAssertion(s"$packageName.$className"))
  }

  def assertCanParseAndCompile(definition: JsonSchema.Definition, packageName: String, className: String): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = packageName)
    val code = JsonSchema2ScalaCodeRenderer.render(className, definition, options, "")
    assertCompiles(code, ClassAssertion(s"$packageName.$className"))
  }

  def assertRenderingFails(schema: String): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = "a.b.c")
    val schemaJson = Json.parse(schema).as[JsObject]
    val definition = JsonSchema.read(schemaJson)
    an[Exception] shouldBe thrownBy {
      JsonSchema2ScalaCodeRenderer
        .render(className = "Test", definition, options, description = "")
    }
  }

}
