package uk.gov.hmrc.jsonschema2scala.schema

import java.net.URI

import play.api.libs.json.{JsLookup, JsObject}

import scala.collection.mutable

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject) => Schema

  def lookup(reference: String, reader: SchemaReader): Option[Schema]

  def uriToPath(reference: String): List[String]

  def isInternal(reference: String): Boolean
}

object CachingReferenceResolver {

  def apply(uri: URI, schema: JsObject, upstreamResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
    new SchemaReferenceResolver {

      val uriString: String = uri.toString
      val cache: mutable.Map[String, Schema] = collection.mutable.Map[String, Schema]()

      override def lookup(reference: String, reader: SchemaReader): Option[Schema] =
        cache
          .get(reference)
          .orElse {
            if (reference.startsWith("#") || reference.startsWith(uriString)) {

              val relative = if (reference.startsWith(uriString)) {
                reference.substring(uriString.length)
              } else reference

              val path: List[String] = relative.split("/").toList

              val result: Option[Schema] = path
                .dropWhile(s => s == "#" || s == "")
                .foldLeft[JsLookup](schema)((s, p) => s \ p)
                .result
                .asOpt[JsObject]
                .map(reader(path.last, _))

              result

            } else None
          }
          .orElse(if (reference.startsWith("#")) None else upstreamResolver.flatMap(_.lookup(reference, reader)))
          .map { s =>
            cache.update(reference, s)
            s
          }

      override def uriToPath(reference: String): List[String] = {
        val uri = URI.create(reference)
        val (root, fragment) =
          (if (uri.isAbsolute) reference.takeWhile(_ != '#') else uriString, "#" + uri.getFragment)
        (root :: fragment.split("/").toList).reverse
      }

      override def isInternal(reference: String): Boolean = {
        val uri2 = URI.create(reference)
        !uri2.isAbsolute || sameOrigin(uri, uri2) || upstreamResolver.exists(_.isInternal(reference))
      }
    }

  def sameOrigin(uri1: URI, uri2: URI): Boolean =
    uri1.getScheme == uri2.getScheme && uri1.getHost == uri2.getHost && uri1.getPath == uri2.getPath
}

object SourceMapReferenceResolver {

  def apply(
    references: Map[String, SchemaSource],
    upstreamResolver: Option[SchemaReferenceResolver] = None): SchemaReferenceResolver =
    new SchemaReferenceResolver {

      override def lookup(reference: String, reader: SchemaReader): Option[Schema] =
        references
          .get(reference)
          .map(s => reader.apply(s.name, s.json))
          .orElse(upstreamResolver.flatMap(_.lookup(reference, reader)))

      override def uriToPath(reference: String): List[String] = {
        val uri2 = URI.create(reference)
        val (root, fragment) = (if (uri2.isAbsolute) reference.takeWhile(_ != '#') else "", "#" + uri2.getRawFragment)
        (root :: fragment.split("/").toList).reverse
      }

      override def isInternal(reference: String): Boolean = false
    }
}
