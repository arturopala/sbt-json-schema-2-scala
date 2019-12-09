package uk.gov.hmrc.jsonschema2scala

import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.jsonschema2scala.JsonSchema.Schema

import scala.util.Random

trait CodeRenderingAssertions extends CompilationAssertions {
  self: Assertions with Matchers =>

  private def randomName: String = "Test_" ++ String.valueOf(Random.alphanumeric.take(6).toArray)

  def assertCanParseAndCompile(schema: String)(implicit compiler: Compiler): Unit =
    assertCanParseAndCompile(schema, packageName = "a.b.c", className = randomName)

  def assertCanParseAndCompile(schema: JsonSchema.Schema)(implicit compiler: Compiler): Unit =
    assertCanParseAndCompile(schema, Map())

  def assertCanParseAndCompile(schema: JsonSchema.Schema, references: Map[String, Schema])(
    implicit compiler: Compiler): Unit = {
    val definition = JsonSchema.read(schema.content, references)
    assertCanParseAndCompile(definition, packageName = "a.b.c", className = schema.className)
  }

  def assertCanParseAndCompile(schema: String, packageName: String, className: String)(
    implicit compiler: Compiler): Unit = {
    val schemaJson = Json.parse(schema).as[JsObject]
    assertCanParseAndCompile(schemaJson, packageName, className)
  }

  def assertCanParseAndCompile(schemaJson: JsObject, packageName: String, className: String)(
    implicit compiler: Compiler): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = packageName)
    val definition = JsonSchema.read(schemaJson)
    val code = JsonSchema2ScalaCodeRenderer.render(className, definition, options, "")
    assertCompiles(packageName, className, code, ClassAssertion(s"$packageName.$className"))
  }

  def assertCanParseAndCompile(definition: JsonSchema.Definition, packageName: String, className: String)(
    implicit compiler: Compiler): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = packageName)
    val code = JsonSchema2ScalaCodeRenderer.render(className, definition, options, "")
    assertCompiles(packageName, className, code, ClassAssertion(s"$packageName.$className"))
  }

  def assertRenderingFails(schema: String): Unit = {
    val options = JsonSchema2ScalaOptions(features = Set(), packageName = "a.b.c")
    val schemaJson = Json.parse(schema).as[JsObject]
    val definition = JsonSchema.read(schemaJson)
    an[Exception] shouldBe thrownBy {
      JsonSchema2ScalaCodeRenderer
        .render(className = randomName, definition, options, description = "")
    }
  }

}
