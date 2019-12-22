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

package uk.gov.hmrc.jsonschema2scala.schema

import java.io.{File, InputStream}
import java.net.URI

import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.jsonschema2scala.schema.SchemaReferenceResolver.encode

import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait SchemaSource {

  def name: String
  def json: JsObject

  lazy val uri: URI = SchemaReader
    .attemptReadId(json)
    .getOrElse(defaultURI)

  def defaultURI: URI
}

case class SchemaFile(file: File) extends SchemaSource {

  val name: String = {
    file.getName
      .split("\\.")
      .head
      .split("[^\\w]")
      .filterNot(_.isEmpty)
      .map(_.toCharArray.splitAt(1) match { case (p, s) => String.valueOf(p.map(_.toUpper)) + String.valueOf(s) })
      .mkString
  }

  val json: JsObject = {
    val source: Source = Source.fromFile(file, "utf-8")
    val json = Try(Json.parse(source.mkString).as[JsObject])
    source.close()
    json match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  override def defaultURI: URI = URI.create(encode(name))
}

case class SchemaResource(inputStream: InputStream, name: String) extends SchemaSource {

  val json: JsObject = {
    val source: Source = Source.fromInputStream(inputStream, "utf-8")
    val json = Try(Json.parse(source.mkString).as[JsObject])
    Try(source.close())
    json match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  override def defaultURI: URI = URI.create(encode(name))

}
