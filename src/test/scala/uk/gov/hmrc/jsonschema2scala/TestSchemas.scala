package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.JsonSchema.{Schema, SchemaResource}

trait TestSchemas {

  val testSchemas: Seq[Schema] = (1 to 16).map(no =>
    SchemaResource(classOf[JsonSchemaSpec].getResourceAsStream(f"/schemas/E$no%02d.schema.json"), f"Entity$no%02d"))

  val testReferences: Map[String, Schema] = testSchemas.map(s => (s.id.getOrElse("unknown"), s)).toMap

}
