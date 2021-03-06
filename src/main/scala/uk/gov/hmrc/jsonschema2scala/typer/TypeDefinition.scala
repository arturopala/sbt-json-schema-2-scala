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

package uk.gov.hmrc.jsonschema2scala.typer

import uk.gov.hmrc.jsonschema2scala.schema.{ObjectSchema, Schema}

case class TypeDefinition(
  name: String,
  path: List[String],
  schema: ObjectSchema,
  nestedTypes: Seq[TypeDefinition] = Seq.empty,
  isInterface: Boolean = false,
  interfaces: Seq[Schema] = Seq.empty,
  subtypes: Seq[Schema] = Seq.empty,
  externalImports: Set[String] = Set.empty,
  forReferenceOnly: Boolean = false,
  collectiveFieldsAggregated: Seq[CollectiveField] = Seq.empty)

object TypeDefinition {

  def changeName(newName: String, typeDef: TypeDefinition): TypeDefinition =
    modifyNestedPaths(typeDef, newName, 0).copy(name = newName)

  def modifyNestedPaths(typeDef: TypeDefinition, newName: String, pos: Int): TypeDefinition = {
    val modifiedNestedTypes = typeDef.nestedTypes
      .map(modifyNestedPaths(_, newName, pos + 1))
      .map(t => t.copy(path = replacePathElement(t.path, newName, pos)))
    typeDef.copy(nestedTypes = modifiedNestedTypes)
  }

  def replacePathElement(path: List[String], element: String, pos: Int): List[String] =
    if (pos < path.length) path.take(pos) ::: element :: path.drop(pos + 1)
    else path

  def modifyPath(fx: List[String] => List[String])(typeDef: TypeDefinition): TypeDefinition = {
    val modifiedNestedTypes = typeDef.nestedTypes.map(modifyPath(fx))
    typeDef.copy(path = fx(typeDef.path), nestedTypes = modifiedNestedTypes)
  }

  def listSchemaUriToTypePath(typeDef: TypeDefinition, excludeReferences: Boolean): Seq[(String, List[String])] =
    (if (excludeReferences && typeDef.forReferenceOnly) Seq.empty
     else Seq((typeDef.schema.uriDecoded, typeDef.name :: typeDef.path))) ++
      typeDef.nestedTypes.flatMap(listSchemaUriToTypePath(_, excludeReferences))

  def listSchemaUriToTypeInterfaces(typeDef: TypeDefinition): Seq[(String, Seq[Schema])] =
    (if (typeDef.isInterface)
       typeDef.subtypes.map(s => (s.uriDecoded, Seq(typeDef.schema)))
     else Seq.empty) ++ typeDef.nestedTypes.flatMap(listSchemaUriToTypeInterfaces)

}
