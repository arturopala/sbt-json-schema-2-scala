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

import java.net
import java.net.URI

import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject, SchemaReferenceResolver) => Schema

  def lookupJson(uri: URI): Option[(JsValue, SchemaReferenceResolver)]

  def lookupSchema(uri: URI, reader: SchemaReader): Option[(Schema, SchemaReferenceResolver)]

  def uriToPath(uri: URI): List[String]

  def resolveUri(uri: URI): URI

  def listKnownUris: List[URI]

  def isInternal(reference: String): Boolean

  def rootSchemaSource: Option[SchemaSource]
}

object SchemaReferenceResolver {

  def apply(
    schemaUri: URI,
    schemaName: String,
    schemaJson: JsObject,
    upstreamResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
    upstreamResolver match {
      case Some(resolver: CachingReferenceResolver) if resolver.schemaSource.uri == schemaUri =>
        resolver

      case _ =>
        val schemaSource = SchemaSourceJsonWithUri(schemaName, schemaUri, schemaJson)
        new CachingReferenceResolver(schemaSource, upstreamResolver)
    }

  def apply(schemaSource: SchemaSource, upstreamResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
    upstreamResolver match {
      case Some(resolver: CachingReferenceResolver) if resolver.schemaSource.uri == schemaSource.uri =>
        resolver

      case _ =>
        new CachingReferenceResolver(schemaSource, upstreamResolver)
    }

  def apply(
    schemaSources: Seq[SchemaSource],
    upstreamResolver: Option[SchemaReferenceResolver] = None): SchemaReferenceResolver = {

    val resolvers: Seq[SchemaReferenceResolver] =
      schemaSources.map(SchemaReferenceResolver(_, None))

    new MultiReferenceResolver(resolvers, false, upstreamResolver)
  }

  final def rootPath(uri: URI): List[String] = "#" :: uri.toString :: Nil

  def isFragmentOnly(uri: URI): Boolean =
    !uri.isAbsolute && (uri.getPath == null || uri.getPath.isEmpty)

  final def pathToReference(path: List[String]): String =
    path.reverse.filterNot(_.isEmpty) match {
      case Nil           => ""
      case x :: Nil      => x
      case x :: y :: Nil => if (y == "#") x else s"$x/$y"
      case x :: xs       => (if (x == "#") "#/" else x) + xs.mkString("/")
    }

  def computeAbsoluteAndRelativeUriString(rootUri: URI, givenUri: URI): (String, String) =
    if (givenUri.isAbsolute) {
      (givenUri.toString, rootUri.relativize(givenUri).toString)
    } else {
      val a = rootUri.resolve(givenUri)
      (a.toString, a.relativize(givenUri).toString)
    }

  def toJsonPointer(relativeReference: String): List[String] =
    relativeReference
      .split("/")
      .filterNot(_.isEmpty)
      .dropWhile(_ == "#")
      .map(s => if (s.contains("%")) net.URLDecoder.decode(s, "utf-8") else s)
      .toList

  def resolveJsonPointer(jsonPointer: List[String], uri: URI)(json: JsObject): JsLookupResult =
    jsonPointer
      .foldLeft[JsLookup](json) { (s, p) =>
        s match {
          case JsLookup(JsDefined(a: JsArray)) =>
            Try(p.toInt)
              .map(i => a \ i)
              .getOrElse(throw new IllegalArgumentException(s"Invalid reference $uri, cannot access $s with '$p'"))
          case _ => s \ p
        }
      }
      .result

  def sameOrigin(uri1: URI, uri2: URI): Boolean =
    uri1.getScheme == uri2.getScheme && uri1.getHost == uri2.getHost && uri1.getPath == uri2.getPath

  final val emptyJsObject = JsObject(Seq())

}

final class CachingReferenceResolver(
  val schemaSource: SchemaSource,
  val upstreamResolver: Option[SchemaReferenceResolver])
    extends SchemaReferenceResolver {

  import SchemaReferenceResolver._

  lazy val rootUriString: String = schemaSource.uri.toString

  lazy val cache: mutable.Map[String, Schema] = collection.mutable.Map[String, Schema]()

  override def lookupJson(uri: URI): Option[(JsValue, SchemaReferenceResolver)] = {

    val isFragment: Boolean = SchemaReferenceResolver.isFragmentOnly(uri)
    val (absolute, relative) = computeAbsoluteAndRelativeUriString(schemaSource.uri, uri)

    {
      if (isFragment || absolute.startsWith(rootUriString)) {

        val jsonPointer: List[String] = toJsonPointer(relative)

        val result: Option[JsValue] =
          resolveJsonPointer(jsonPointer, uri)(schemaSource.json)
            .asOpt[JsValue]

        result
          .map(v => (v, this))

      } else None
    }.orElse {
      if (isFragment) None else upstreamResolver.flatMap(_.lookupJson(uri))
    }
  }

  override def lookupSchema(uri: URI, reader: SchemaReader): Option[(Schema, SchemaReferenceResolver)] = {

    val isFragmentOnly: Boolean = SchemaReferenceResolver.isFragmentOnly(uri)
    val (absolute, relative) = computeAbsoluteAndRelativeUriString(schemaSource.uri, uri)

    def read(jsObject: JsObject, name: String): Schema = {
      // prevent cycles by caching a schema stub
      val stub = SchemaStub(reader(name, emptyJsObject, this), absolute)
      cache.update(absolute, stub)

      val schema: Schema = reader(name, jsObject, this)

      cache.update(absolute, schema)
      schema
    }

    cache
      .get(absolute)
      .orElse {
        if (isFragmentOnly || absolute.startsWith(rootUriString)) {

          val jsonPointer: List[String] = toJsonPointer(relative)

          val name = if (jsonPointer.isEmpty) schemaSource.name else jsonPointer.last

          val result: Option[Schema] = resolveJsonPointer(jsonPointer, uri)(schemaSource.json)
            .asOpt[JsValue]
            .flatMap {
              case jsObject: JsObject                      => Some(jsObject)
              case jsArray: JsArray                        => Some(Json.obj("items" -> jsArray))
              case jsBoolean: JsBoolean if jsBoolean.value => Some(Json.obj())
              case _                                       => None
            }
            .map(read(_, name))

          result

        } else None
      }
      .map((_, this))
      .orElse {
        if (isFragmentOnly) None else upstreamResolver.flatMap(_.lookupSchema(uri, reader))
      }
  }

  override def uriToPath(uri: URI): List[String] = {
    val (root, fragment) =
      (
        if (uri.isAbsolute) uri.toString.takeWhile(_ != '#') else rootUriString,
        "#" + Option(uri.getFragment).getOrElse(""))
    (root :: fragment.split("/").toList).reverse
  }

  override def isInternal(reference: String): Boolean = {
    val uri2 = URI.create(reference)
    !uri2.isAbsolute || sameOrigin(schemaSource.uri, uri2) || upstreamResolver.exists(_.isInternal(reference))
  }

  override def resolveUri(givenUri: URI): URI = schemaSource.uri.resolve(givenUri)

  override def listKnownUris: List[URI] =
    schemaSource.uri :: upstreamResolver.map(_.listKnownUris).getOrElse(Nil)

  @tailrec
  def rootSchemaSource: Option[SchemaSource] = upstreamResolver match {
    case Some(r: CachingReferenceResolver) => r.rootSchemaSource
    case _                                 => Some(schemaSource)
  }

  override def toString: String = s"CachingReferenceResolver for ${schemaSource.uri}"
}

final class MultiReferenceResolver(
  resolvers: Seq[SchemaReferenceResolver],
  internal: Boolean,
  upstreamResolver: Option[SchemaReferenceResolver])
    extends SchemaReferenceResolver {

  override def lookupJson(uri: URI): Option[(JsValue, SchemaReferenceResolver)] =
    resolvers
      .foldLeft[Option[(JsValue, SchemaReferenceResolver)]](None)(
        (a, r) =>
          a.orElse(
            r.lookupJson(uri)
        ))
      .orElse(upstreamResolver.flatMap(_.lookupJson(uri)))

  override def lookupSchema(uri: URI, reader: SchemaReader): Option[(Schema, SchemaReferenceResolver)] =
    resolvers
      .foldLeft[Option[(Schema, SchemaReferenceResolver)]](None)(
        (a, r) =>
          a.orElse(
            r.lookupSchema(uri, reader)
        ))
      .orElse(upstreamResolver.flatMap(_.lookupSchema(uri, reader)))

  override def uriToPath(uri: URI): List[String] = {
    val (root, fragment) = (
      if (uri.isAbsolute) uri.toString.takeWhile(_ != '#')
      else "",
      "#" + Option(uri.getFragment).getOrElse(""))
    (root :: fragment.split("/").toList).reverse
  }

  override def isInternal(reference: String): Boolean = internal

  override def resolveUri(uri: URI): URI = uri

  override def listKnownUris: List[URI] =
    resolvers.flatMap(_.listKnownUris).toList ::: upstreamResolver.map(_.listKnownUris).getOrElse(Nil)

  def rootSchemaSource: Option[SchemaSource] = upstreamResolver match {
    case Some(r: CachingReferenceResolver) => r.rootSchemaSource
    case _                                 => None
  }

  override def toString: String = s"MultiReferenceResolver of size [${resolvers.size}]"
}
