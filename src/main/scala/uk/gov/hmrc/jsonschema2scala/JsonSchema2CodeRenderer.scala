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

package uk.gov.hmrc.jsonschema2scala

import uk.gov.hmrc.jsonschema2scala.JsonSchema._

case class TypeDefinition(
  name: String,
  definition: ObjectDefinition,
  nestedTypes: Seq[TypeDefinition] = Seq.empty,
  prefix: String,
  isInterface: Boolean = false,
  interfaces: Seq[TypeDefinition] = Seq.empty,
  subtypes: Seq[TypeDefinition] = Seq.empty,
  interfaceMethods: Set[(String, String)] = Set.empty,
  externalImports: Set[String] = Set.empty) {

  def hasInterfaces: Boolean = interfaces.nonEmpty
}

trait JsonSchema2CodeRenderer {

  def typeName(definition: Definition): String

  def render(
    className: String,
    typeDef: TypeDefinition,
    options: JsonSchema2ScalaOptions,
    description: String): Seq[Code]

  final def render(
    className: String,
    definition: Definition,
    options: JsonSchema2ScalaOptions,
    description: String): Seq[Code] =
    findObjectDefinition(definition).fold(
      throw new IllegalArgumentException("Provided json schema does not represent valid object definition")
    ) { definition =>
      val typeDef = moveRefTypesToTheTop(typeDefinition(className, definition))
        .copy(externalImports = calculateExternalImports(definition))
      render(className, typeDef, options, description)
    }

  private def findObjectDefinition(definition: Definition): Option[ObjectDefinition] = definition match {
    case o: ObjectDefinition => Some(o)
    case o: OneOfDefinition  => o.variants.map(findObjectDefinition).collectFirst { case Some(x) => x }
    case _                   => None
  }

  private def moveRefTypesToTheTop(typeDef: TypeDefinition): TypeDefinition = {
    val refTypesMap: Map[String, TypeDefinition] = collectRefTypes(typeDef)
      .map(t => t.copy(prefix = ""))
      .groupBy(_.definition.path)
      .mapValues(_.reduce((a, b) => a.copy(interfaces = a.interfaces ++ b.interfaces)))

    val commonRefTypes = refTypesMap.values.toSeq.filterNot(_.definition == typeDef.definition)

    val typeDef1: TypeDefinition = removeNestedRefTypes(typeDef)
    typeDef.copy(nestedTypes = (commonRefTypes ++ typeDef1.nestedTypes).sortBy(t => typeName(t.definition)))
  }

  private def collectRefTypes(typeDef: TypeDefinition): Seq[TypeDefinition] =
    (if (typeDef.definition.isRef) Seq(typeDef) else Seq.empty) ++ typeDef.nestedTypes.flatMap(collectRefTypes)

  private def removeNestedRefTypes(typeDef: TypeDefinition): TypeDefinition =
    if (typeDef.isInterface) typeDef
    else
      typeDef.copy(
        nestedTypes = typeDef.nestedTypes.filter(!_.definition.isRef).map(removeNestedRefTypes)
      )

  private def typeDefinition(name: String, definition: ObjectDefinition, prefix: String = ""): TypeDefinition =
    TypeDefinition(
      name,
      definition,
      definition.properties.collect {
        case od: ObjectDefinition => Seq(typeDefinition(typeName(od), od, s"${typeName(od)}."))
        case oneOf: OneOfDefinition if oneOf.variants.collect { case _: ObjectDefinition => }.nonEmpty =>
          val subtypes = oneOf.variants
            .collect { case o: ObjectDefinition => o }
            .zipWithIndex
            .map {
              case (od2, pos) => {
                val name = if (typeName(od2) == typeName(oneOf)) s"${typeName(od2)}_$pos" else typeName(od2)
                typeDefinition(name, od2, s"$name.")
              }
            }
          if (subtypes.size <= 1) subtypes
          else {
            // New artificial interface type to span over multiple oneOf variants
            //val isRef = oneOf.isRef || subtypes.exists(_.definition.isRef)
            val superType = TypeDefinition(
              typeName(oneOf),
              ObjectDefinition(
                oneOf.name,
                oneOf.path,
                Seq.empty,
                Seq.empty,
                oneOf.isRef,
                oneOf.description,
                oneOf.isMandatory),
              prefix = if (oneOf.isRef) "" else prefix,
              isInterface = true,
              interfaceMethods = findInterfaceMethods(subtypes)
            )
            val subtypes2 = subtypes.map(s => s.copy(interfaces = s.interfaces :+ superType))
            Seq(superType.copy(subtypes = subtypes2, nestedTypes = subtypes2))
          }
        case a: ArrayDefinition if a.item.isInstanceOf[ObjectDefinition] =>
          Seq(
            typeDefinition(
              typeName(a.item.asInstanceOf[ObjectDefinition]),
              a.item.asInstanceOf[ObjectDefinition],
              s"${typeName(a.item.asInstanceOf[ObjectDefinition])}."))
      }.flatten,
      prefix
    )

  private def findInterfaceMethods(subtypes: Seq[TypeDefinition]): Set[(String, String)] =
    subtypes
      .map(_.definition)
      .map {
        case o: ObjectDefinition => o.properties.map(d => (d.name, typeOf(d, ""))).toSet
        case _                   => Set.empty[(String, String)]
      }
      .reduce[Set[(String, String)]]((a, b) => a.intersect(b))

  protected def typeOf(
    definition: Definition,
    prefix: String,
    wrapOption: Boolean = true,
    defaultValue: Boolean = true): String = {
    val name = definition match {
      case _: StringDefinition  => "String"
      case _: NumberDefinition  => "BigDecimal"
      case _: BooleanDefinition => "Boolean"
      case a: ArrayDefinition   => s"Seq[${typeOf(a.item, prefix, wrapOption, defaultValue)}]"
      case o: ObjectDefinition  => s"${if (o.isRef) "" else prefix}${typeName(o)}"
      case o: OneOfDefinition =>
        if (o.variants.isEmpty) "Nothing"
        else if (o.variants.size == 1) typeOf(o.variants.head, prefix, wrapOption = false)
        else
          s"${if (o.isRef) "" else prefix}${typeName(o)}"
      case e: ExternalDefinition =>
        e.`type` match {
          case Some("object")  => s"${if (e.isRef) "" else prefix}${typeName(e)}"
          case Some("string")  => "String"
          case Some("number")  => "BigDecimal"
          case Some("boolean") => "Boolean"
          case Some(t)         => throw new Exception(s"Unsupported reference type $t")
          case None            => throw new Exception(s"Missing reference type")
        }
    }
    if (!definition.isMandatory && wrapOption) s"Option[$name]${if (defaultValue) " = None" else ""}"
    else { s"""$name ${if (defaultValue && definition.isBoolean) " = false" else ""}""" }
  }

  def calculateExternalImports(definition: JsonSchema.ObjectDefinition): Set[String] =
    definition.properties.flatMap {
      case o: ObjectDefinition => calculateExternalImports(o)
      case oneOf: OneOfDefinition if oneOf.variants.collect { case _: ObjectDefinition => }.nonEmpty =>
        oneOf.variants.collect { case o: ObjectDefinition => o }.flatMap(calculateExternalImports)
      case a: ArrayDefinition if a.item.isInstanceOf[ExternalDefinition] =>
        Set(typeName(a.item.asInstanceOf[ObjectDefinition]))
      case e: ExternalDefinition if !e.isPrimitive =>
        Set(typeName(e))
      case _ => Set()
    }.toSet
}
