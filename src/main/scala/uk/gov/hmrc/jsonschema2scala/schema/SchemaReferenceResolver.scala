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

import java.net.URI

import play.api.libs.json.{JsLookup, JsLookupResult, JsObject, JsValue}

import scala.collection.mutable

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject, SchemaReferenceResolver) => Schema

  def lookupJson(reference: String): Option[(JsValue, SchemaReferenceResolver)]

  def lookupSchema(reference: String, reader: SchemaReader): Option[Schema]

  def uriToPath(reference: String): List[String]

  def isInternal(reference: String): Boolean

  def resolveUri(uri: URI): URI

  def listKnownUri: List[URI]
}

object SchemaReferenceResolver {

  final def rootPath(uri: URI): List[String] = "#" :: uri.toString :: Nil

  final def pathToUri(path: List[String]): String =
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

  def toJsonPointer(relative: String): List[String] =
    relative
      .split("/")
      .filterNot(_.isEmpty)
      .dropWhile(_ == "#")
      .toList

  def resolveJsonPointer(jsonPointer: List[String])(json: JsObject): JsLookupResult =
    jsonPointer
      .foldLeft[JsLookup](json)((s, p) => s \ p)
      .result

  def sameOrigin(uri1: URI, uri2: URI): Boolean =
    uri1.getScheme == uri2.getScheme && uri1.getHost == uri2.getHost && uri1.getPath == uri2.getPath

  final val emptyJsObject = JsObject(Seq())

}

object CachingReferenceResolver {

  import SchemaReferenceResolver._

  def apply(
    rootUri: URI,
    schemaName: String,
    schema: JsObject,
    upstreamResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
    upstreamResolver match {
      case Some(resolver @ CachingReferenceResolver(`rootUri`, _, _, _)) =>
        resolver
      case _ =>
        CachingReferenceResolver(rootUri, schemaName, schema, upstreamResolver)
    }

  case class CachingReferenceResolver(
    rootUri: URI,
    schemaName: String,
    json: JsObject,
    upstreamResolver: Option[SchemaReferenceResolver])
      extends SchemaReferenceResolver {

    lazy val rootUriString: String = rootUri.toString

    lazy val cache: mutable.Map[String, Schema] = collection.mutable.Map[String, Schema]()

    override def lookupJson(reference: String): Option[(JsValue, SchemaReferenceResolver)] = {

      val isFragment: Boolean = reference.startsWith("#")
      val uri: URI = URI.create(reference)
      val (absolute, relative) = computeAbsoluteAndRelativeUriString(rootUri, uri)

      {
        if (isFragment || absolute.startsWith(rootUriString)) {

          val jsonPointer: List[String] = toJsonPointer(relative)

          val result: Option[JsValue] =
            resolveJsonPointer(jsonPointer)(json)
              .asOpt[JsValue]

          result
            .map(v => (v, this))

        } else None
      }.orElse {
        if (isFragment) None else upstreamResolver.flatMap(_.lookupJson(reference))
      }
    }

    override def lookupSchema(reference: String, reader: SchemaReader): Option[Schema] = {

      val isFragment: Boolean = reference.startsWith("#")
      val uri: URI = URI.create(reference)
      val (absolute, relative) = computeAbsoluteAndRelativeUriString(rootUri, uri)

      cache
        .get(absolute)
        .orElse {
          if (isFragment || absolute.startsWith(rootUriString)) {

            val jsonPointer: List[String] = toJsonPointer(relative)

            val name = if (jsonPointer.isEmpty) schemaName else jsonPointer.last

            val result: Option[Schema] = resolveJsonPointer(jsonPointer)(json)
              .asOpt[JsObject]
              .map { schemaJson =>
                // prevent cycles by caching a schema stub
                val stub = SchemaStub(reader(name, emptyJsObject, this), absolute)
                cache.update(absolute, stub)

                val schema: Schema = reader(name, schemaJson, this)

                cache.update(absolute, schema)
                schema
              }

            result

          } else None
        }
        .orElse {
          if (isFragment) None else upstreamResolver.flatMap(_.lookupSchema(reference, reader))
        }
    }

    override def uriToPath(reference: String): List[String] = {
      val uri = URI.create(reference)
      val (root, fragment) =
        (
          if (uri.isAbsolute) reference.takeWhile(_ != '#') else rootUriString,
          "#" + Option(uri.getFragment).getOrElse(""))
      (root :: fragment.split("/").toList).reverse
    }

    override def isInternal(reference: String): Boolean = {
      val uri2 = URI.create(reference)
      !uri2.isAbsolute || sameOrigin(rootUri, uri2) || upstreamResolver.exists(_.isInternal(reference))
    }

    override def resolveUri(givenUri: URI): URI = rootUri.resolve(givenUri)

    override def listKnownUri: List[URI] =
      rootUri :: upstreamResolver.map(_.listKnownUri).getOrElse(Nil)
  }
}

object MultiSourceReferenceResolver {

  def apply(
    schemaSources: Seq[SchemaSource],
    upstreamResolver: Option[SchemaReferenceResolver] = None): SchemaReferenceResolver =
    new SchemaReferenceResolver {

      lazy val resolvers: Seq[SchemaReferenceResolver] =
        schemaSources.map(s => CachingReferenceResolver(s.uri, s.name, s.json, None))

      override def lookupJson(reference: String): Option[(JsValue, SchemaReferenceResolver)] =
        resolvers
          .foldLeft[Option[(JsValue, SchemaReferenceResolver)]](None)(
            (a, r) =>
              a.orElse(
                r.lookupJson(reference)
            ))
          .orElse(upstreamResolver.flatMap(_.lookupJson(reference)))

      override def lookupSchema(reference: String, reader: SchemaReader): Option[Schema] =
        resolvers
          .foldLeft[Option[Schema]](None)(
            (a, r) =>
              a.orElse(
                r.lookupSchema(reference, reader)
            ))
          .orElse(upstreamResolver.flatMap(_.lookupSchema(reference, reader)))

      override def uriToPath(reference: String): List[String] = {
        val uri2 = URI.create(reference)
        val (root, fragment) =
          (if (uri2.isAbsolute) reference.takeWhile(_ != '#') else "", "#" + Option(uri2.getFragment).getOrElse(""))
        (root :: fragment.split("/").toList).reverse
      }

      override def isInternal(reference: String): Boolean = false

      override def resolveUri(uri: URI): URI = uri

      override def listKnownUri: List[URI] =
        resolvers.flatMap(_.listKnownUri).toList ::: upstreamResolver.map(_.listKnownUri).getOrElse(Nil)
    }
}
