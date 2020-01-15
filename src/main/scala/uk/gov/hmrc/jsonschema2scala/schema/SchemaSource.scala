/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.jsonschema2scala.schema

import java.io.{File, InputStream}
import java.net.URI

import play.api.libs.json.{JsObject, Json}

import scala.io.Source
import scala.util.{Failure, Try}

sealed trait SchemaSource {

  def name: String
  def uri: URI
  def json: JsObject
}

object SchemaSource {

  def apply(name: String, inputStream: InputStream): SchemaSource = {
    val content: String = {
      val source: Source = Source.fromInputStream(inputStream, "utf-8")
      readSource(source)
    }
    SchemaSourceUnparsed(name, content)
  }

  def readSource(source: Source): String =
    Try(removeBOM(source.mkString)).recoverWith {
      case e =>
        Try(source.close())
        Failure(e)
    }.get

  private def removeBOM(s: String): String =
    if (s.startsWith("\uFEFF")) s.substring(1) else s

}

case class SchemaSourceJsonWithUri(name: String, uri: URI, json: JsObject) extends SchemaSource

case class SchemaSourceJson(name: String, json: JsObject) extends SchemaSource with SchemaUriReader

case class SchemaSourceUnparsed(name: String, content: String)
    extends SchemaSource with SchemaUriReader with SchemaContentParser

case class SchemaSourceFile(file: File) extends SchemaSource with SchemaUriReader with SchemaContentParser {

  override val name: String = {
    file.getName
      .split("\\.")
      .head
      .split("[^\\w]")
      .filterNot(_.isEmpty)
      .map(_.toCharArray.splitAt(1) match { case (p, s) => String.valueOf(p.map(_.toUpper)) + String.valueOf(s) })
      .mkString
  }

  override val content: String = {
    val source: Source = Source.fromFile(file, "utf-8")
    SchemaSource.readSource(source)
  }
}

trait SchemaContentParser {

  def content: String

  private lazy val parseResult: Either[Throwable, JsObject] = {
    Try(Json.parse(content).as[JsObject]).toEither
  }

  lazy val json: JsObject = parseResult.fold(throw _, identity)

}

trait SchemaUriReader {

  def name: String
  def json: JsObject

  lazy val uri: URI = SchemaReader.attemptReadId(json).getOrElse(URI.create(name))

}
