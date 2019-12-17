package uk.gov.hmrc.jsonschema2scala.schema

import play.api.libs.json.{JsLookup, JsObject}

trait SchemaReferenceResolver {

  type SchemaReader = (String, JsObject) => Schema

  def lookup(reference: String, reader: SchemaReader): Option[Schema]
}

object CachingReferenceResolver {

  val cache = collection.mutable.Map[String, Schema]()

  def apply(schema: JsObject, externalResolver: Option[SchemaReferenceResolver]): SchemaReferenceResolver =
    new SchemaReferenceResolver {
      override def lookup(reference: String, reader: SchemaReader): Option[Schema] =
        cache
          .get(reference)
          .orElse {
            if (reference.startsWith("#/")) {
              val path: List[String] = reference.split("/").toList
              path
                .drop(1)
                .foldLeft[JsLookup](schema)((s, p) => s \ p)
                .result
                .asOpt[JsObject]
                .map(reader(path.last, _))
            } else None
          }
          .orElse(externalResolver.flatMap(_.lookup(reference, reader)))
          .map { s =>
            cache.update(reference, s)
            s
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
          .map(s => reader.apply(s.name, s.content))
          .orElse(externalResolver.flatMap(_.lookup(reference, reader)))
    }
}
