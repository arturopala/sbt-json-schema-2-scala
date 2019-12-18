package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.schema.ObjectSchema

case class TypeDefinition(
  name: String,
  path: List[String],
  schema: ObjectSchema,
  nestedTypes: Seq[TypeDefinition] = Seq.empty,
  isInterface: Boolean = false,
  interfaces: Seq[TypeDefinition] = Seq.empty,
  subtypes: Seq[TypeDefinition] = Seq.empty,
  externalImports: Set[String] = Set.empty,
  forReferenceOnly: Boolean = false)

object TypeDefinition {

  def listSchemaUriToTypePath(typeDef: TypeDefinition): Seq[(String, List[String])] =
    Seq((typeDef.schema.uri, typeDef.name :: typeDef.path)) ++ typeDef.nestedTypes.flatMap(listSchemaUriToTypePath)

  def listSchemaUriToTypeInterfaces(typeDef: TypeDefinition): Seq[(String, Seq[List[String]])] =
    (if (typeDef.isInterface)
       typeDef.subtypes.filter(_.forReferenceOnly).map(i => (i.schema.uri, Seq(typeDef.name :: typeDef.path)))
     else Seq.empty) ++ typeDef.nestedTypes.flatMap(listSchemaUriToTypeInterfaces)

}
