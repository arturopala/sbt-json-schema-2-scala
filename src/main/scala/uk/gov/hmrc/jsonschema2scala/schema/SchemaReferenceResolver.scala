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

import play.api.libs.json.{JsLookup, JsObject}

import scala.collection.mutable

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject) => Schema

  def lookup(reference: String, reader: SchemaReader): Option[Schema]

  def uriToPath(reference: String): List[String]

  def isInternal(reference: String): Boolean

  def resolveUri(uri: URI): URI
}

object SchemaReferenceResolver {

  def rootPath(uri: URI): List[String] = "#" :: uri.toString :: Nil

  def pathToUri(path: List[String]): String =
    path.reverse.filterNot(_.isEmpty) match {
      case Nil           => ""
      case x :: Nil      => x
      case x :: y :: Nil => if (y == "#") x else s"$x/$y"
      case x :: xs       => (if (x == "#") "#/" else x) + xs.mkString("/")
    }

}

object CachingReferenceResolver {

  def apply(
    rootUri: URI,
    schemaName: String,
    schema: JsObject,
    upstreamResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver = upstreamResolver match {
    case Some(resolver @ CachingReferenceResolver(`rootUri`, _, _, _)) => resolver
    case _                                                             => CachingReferenceResolver(rootUri, schemaName, schema, upstreamResolver)
  }

  case class CachingReferenceResolver(
    rootUri: URI,
    schemaName: String,
    json: JsObject,
    upstreamResolver: Option[SchemaReferenceResolver])
      extends SchemaReferenceResolver {

    val rootUriString: String = rootUri.toString

    val cache: mutable.Map[String, Schema] = collection.mutable.Map[String, Schema]()

    override def lookup(reference: String, reader: SchemaReader): Option[Schema] = {

      val isFragment: Boolean = reference.startsWith("#")

      val uri: URI = URI.create(reference)

      val (absolute, relative) = if (uri.isAbsolute) {
        (uri.toString, rootUri.relativize(uri).toString)
      } else {
        val a = rootUri.resolve(uri)
        (a.toString, a.relativize(uri).toString)
      }

      cache
        .get(absolute)
        .orElse {
          if (isFragment || absolute.startsWith(rootUriString)) {

            val jsonPointer: List[String] = relative
              .split("/")
              .filterNot(_.isEmpty)
              .dropWhile(_ == "#")
              .toList

            val name = if (jsonPointer.isEmpty) schemaName else jsonPointer.last

            val result: Option[Schema] = jsonPointer
              .foldLeft[JsLookup](json)((s, p) => s \ p)
              .result
              .asOpt[JsObject]
              .map { schemaJson =>
                // prevent cycles by caching a schema stub
                val stub = SchemaStub(reader(name, emptyJsObject), absolute)
                cache.update(absolute, stub)

                val schema: Schema = reader(name, schemaJson)

                cache.update(absolute, schema)
                schema
              }

            result

          } else None
        }
        .orElse {
          if (isFragment) None else upstreamResolver.flatMap(_.lookup(reference, reader))
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
  }

  def sameOrigin(uri1: URI, uri2: URI): Boolean =
    uri1.getScheme == uri2.getScheme && uri1.getHost == uri2.getHost && uri1.getPath == uri2.getPath

  val emptyJsObject = JsObject(Seq())
}

object MultiSourceReferenceResolver {

  def apply(
    schemaSources: Seq[SchemaSource],
    upstreamResolver: Option[SchemaReferenceResolver] = None): SchemaReferenceResolver =
    new SchemaReferenceResolver {

      lazy val resolvers: Seq[SchemaReferenceResolver] =
        schemaSources.map(s => CachingReferenceResolver(s.uri, s.name, s.json, None))

      override def lookup(reference: String, reader: SchemaReader): Option[Schema] =
        resolvers
          .foldLeft[Option[Schema]](None)(
            (a, r) =>
              a.orElse(
                r.lookup(reference, reader)
            ))
          .orElse(upstreamResolver.flatMap(_.lookup(reference, reader)))

      override def uriToPath(reference: String): List[String] = {
        val uri2 = URI.create(reference)
        val (root, fragment) =
          (if (uri2.isAbsolute) reference.takeWhile(_ != '#') else "", "#" + Option(uri2.getFragment).getOrElse(""))
        (root :: fragment.split("/").toList).reverse
      }

      override def isInternal(reference: String): Boolean = false

      override def resolveUri(uri: URI): URI = uri
    }
}
