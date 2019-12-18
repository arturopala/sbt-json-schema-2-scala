package uk.gov.hmrc.jsonschema2scala.schema

import java.net.URI

import play.api.libs.json.{JsLookup, JsObject}

import scala.collection.mutable

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject) => Schema

  def lookup(reference: String, reader: SchemaReader): Option[Schema]

  def parse(uri: String): List[String]
}

object CachingReferenceResolver {

  def apply(uri: URI, schema: JsObject, externalResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
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
          .orElse(if (reference.startsWith("#")) None else externalResolver.flatMap(_.lookup(reference, reader)))
          .map { s =>
            cache.update(reference, s)
            s
          }

      override def parse(uri: String): List[String] = {
        val uri2 = URI.create(uri)
        val (root, fragment) =
          (if (uri2.isAbsolute) uri.takeWhile(_ != '#') else uriString, "#" + uri2.getFragment)
        (root :: fragment.split("/").toList).reverse
      }
    }
}

object SourceMapReferenceResolver {

  def apply(
    references: Map[String, SchemaSource],
    externalResolver: Option[SchemaReferenceResolver] = None): SchemaReferenceResolver =
    new SchemaReferenceResolver {

      override def lookup(reference: String, reader: SchemaReader): Option[Schema] =
        references
          .get(reference)
          .map(s => reader.apply(s.name, s.json))
          .orElse(externalResolver.flatMap(_.lookup(reference, reader)))

      override def parse(uri: String): List[String] = {
        val uri2 = URI.create(uri)
        val (root, fragment) = (if (uri2.isAbsolute) uri.takeWhile(_ != '#') else "", "#" + uri2.getRawFragment)
        (root :: fragment.split("/").toList).reverse
      }
    }
}
