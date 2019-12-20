/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.jsonschema2scala

import java.net.URI

import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.jsonschema2scala.generator.scala2.{ScalaCodeGenerator, ScalaCodeGeneratorOptions}
import uk.gov.hmrc.jsonschema2scala.schema.{Schema, SchemaReader, SchemaSource}

import scala.util.Random

trait CodeRenderingAssertions extends CompilationAssertions {
  self: Assertions with Matchers =>

  private def randomName: String = "Test" ++ String.valueOf(Random.alphanumeric.take(6).toArray)

  def assertCanParseAndCompile(schemaString: String)(implicit compiler: Compiler): Unit =
    assertCanParseAndCompile(schemaString, packageName = "a.b.c", className = randomName)

  def assertCanParseAndCompile(schemaSource: SchemaSource)(implicit compiler: Compiler): Unit =
    assertCanParseAndCompile(schemaSource)

  def assertCanParseAndCompile(schemaSource: SchemaSource, references: Map[String, SchemaSource] = Map())(
    implicit compiler: Compiler): Unit = {
    val definition = SchemaReader.read(schemaSource.uri, schemaSource.name, schemaSource.json, references)
    assertCanParseAndCompile(definition, packageName = "a.b.c")
  }

  def assertCanParseAndCompile(schemaString: String, packageName: String, className: String)(
    implicit compiler: Compiler): Unit = {
    val schemaJson = Json.parse(schemaString).as[JsObject]
    assertCanParseAndCompile(schemaJson, packageName, className)
  }

  def assertCanParseAndCompile(schemaJson: JsObject, packageName: String, className: String)(
    implicit compiler: Compiler): Unit = {
    val options = ScalaCodeGeneratorOptions(features = Set(), packageName = packageName)
    val definition = SchemaReader.read(className, schemaJson)
    val result = ScalaCodeGenerator.generateCodeFrom(definition, options, "")
    assertSuccessAndCompiles(result)
  }

  def assertCanParseAndCompile(schema: Schema, packageName: String)(implicit compiler: Compiler): Unit = {
    val options = ScalaCodeGeneratorOptions(features = Set(), packageName = packageName)
    val result = ScalaCodeGenerator.generateCodeFrom(schema, options, "")
    assertSuccessAndCompiles(result)
  }

  def assertRenderingFails(schemaString: String): Unit = {
    val options = ScalaCodeGeneratorOptions(features = Set(), packageName = "a.b.c")
    val schemaJson = Json.parse(schemaString).as[JsObject]
    val name = randomName
    val definition = SchemaReader.read(name, schemaJson)
    ScalaCodeGenerator
      .generateCodeFrom(definition, options, description = "") should be leftSide
  }

}
