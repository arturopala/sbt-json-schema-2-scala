package uk.gov.hmrc.jsonschema2scala.schema

import java.io.{File, InputStream}

import play.api.libs.json.{JsObject, Json}

import scala.io.Source
import scala.util.{Failure, Success, Try}

sealed trait SchemaSource {

  def name: String
  def content: JsObject

  lazy val id: Option[String] = (content \ "$id").asOpt[String]
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

  val content: JsObject = {
    val source: Source = Source.fromFile(file, "utf-8")
    val json = Try(Json.parse(source.mkString).as[JsObject])
    source.close()
    json match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }
}

case class SchemaResource(inputStream: InputStream, name: String) extends SchemaSource {

  val content: JsObject = {
    val source: Source = Source.fromInputStream(inputStream, "utf-8")
    val json = Try(Json.parse(source.mkString).as[JsObject])
    Try(source.close())
    json match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

}
