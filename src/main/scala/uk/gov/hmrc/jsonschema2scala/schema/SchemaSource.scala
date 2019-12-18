package uk.gov.hmrc.jsonschema2scala.schema

import java.io.{File, InputStream}
import java.net.URI

import play.api.libs.json.{JsObject, Json}

import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait SchemaSource {

  def name: String
  def json: JsObject

  lazy val uri: URI = SchemaReader
    .readId(json)
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

  override def defaultURI: URI = URI.create(s"schema://$name")
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

  override def defaultURI: URI = URI.create(s"schema://$name")

}
