package uk.gov.hmrc.jsonschema2scala.schema

object SchemaUtils {

  def copy(schema: Schema, name: String, path: List[String], description: Option[String]): Schema = schema match {
    case s: ObjectSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: ArraySchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: MapSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: OneOfSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: StringSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: NumberSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: IntegerSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: BooleanSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: NullSchema =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: InternalSchemaReference =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
    case s: ExternalSchemaReference =>
      s.copy(name = name, path = path, description = description.orElse(schema.description))
  }
}
