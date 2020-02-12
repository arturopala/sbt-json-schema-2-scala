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

package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.generator.TypeResolver
import uk.gov.hmrc.jsonschema2scala.utils.NameUtils.{firstCharLowercase, normalize}
import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.typer.{NameProvider, TypeDefinition}

object ScalaCodeGeneratorContext {

  def apply(schema: Schema, options: ScalaCodeGeneratorOptions)(
    implicit nameProvider: NameProvider): ScalaCodeGeneratorContext = {

    val uniqueKey = findUniqueKey(schema)
    val keys = findKeys(schema)

    val externalizedStrings = collectCommonValues(schema)
      .groupBy(_._1)
      .mapValues(_.map(_._2).map(_.replaceAll("\\d", "")).distinct.minBy(_.length))
      .groupBy(_._2)
      .mapValues(m => if (m.size <= 1) m else m.toSeq.zipWithIndex.map { case ((k, v), i) => (k, v + i) }.toMap)
      .foldLeft[Map[String, String]](Map())((a, v) => a ++ v._2)

    ScalaCodeGeneratorContext(options, uniqueKey, keys, externalizedStrings)
  }

  private def findUniqueKey(schema: Schema, path: List[Schema] = Nil)(
    implicit nameProvider: NameProvider): Option[(String, String)] =
    (schema match {
      case s: StringSchema => if (s.hasCustom("x_uniqueKey")) Some((accessorFor(s :: path), s.name)) else None
      case o: ObjectSchema =>
        o.properties.foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, o :: path)))
      case _ => None
    }).orElse(schema.definitions
      .foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, path))))

  private def findKeys(schema: Schema, path: List[Schema] = Nil)(
    implicit nameProvider: NameProvider): Seq[(String, String)] =
    (schema match {
      case s: StringSchema => if (s.hasCustom("x_key")) Seq((accessorFor(s :: path), s.name)) else Seq.empty
      case o: ObjectSchema =>
        o.properties.flatMap(findKeys(_, o :: path))
      case _ => Seq.empty
    }) ++ schema.definitions.flatMap(s => findKeys(s, s :: path))

  private def accessorFor(path: List[Schema], nested: String = "", option: Boolean = false)(
    implicit nameProvider: NameProvider): String = path match {
    case (o: ObjectSchema) :: xs =>
      val prefix =
        if (o.name.isEmpty) ""
        else if (o.required) s"${nameProvider.toIdentifier(o.name)}."
        else s"${o.name}.${if (option) "flatMap" else "map"}(_."
      val suffix = if (o.name.isEmpty) "" else if (!o.required) ")" else ""
      accessorFor(xs, prefix + nested + suffix, !o.required || option)
    case (s: Schema) :: xs =>
      accessorFor(xs, s.name, !s.required)
    case Nil => if (option) nested else s"Option($nested)"
  }

  private def externalizePattern(schema: StringSchema)(implicit nameProvider: NameProvider): Seq[(String, String)] =
    schema.pattern
      .map(p => {
        val key = quoted(p)
        (key, s"${nameProvider.toIdentifier(schema.name + "Pattern")}")
      })
      .toSeq

  private def externalizeEnum(schema: StringSchema)(implicit nameProvider: NameProvider): Seq[(String, String)] =
    schema.enum
      .map(e => (s"""Seq(${e.mkString("\"", "\",\"", "\"")})""", s"${nameProvider.toIdentifier(schema.name + "Enum")}"))
      .toSeq

  private def collectCommonValues(schema: Schema)(implicit nameProvider: NameProvider): Seq[(String, String)] =
    schema.definitions.flatMap(collectCommonValues) ++
      (schema match {
        case stringSchema: StringSchema =>
          externalizePattern(stringSchema) ++ externalizeEnum(stringSchema)
        case objectSchema: ObjectSchema =>
          val m1 = objectSchema.properties.flatMap(collectCommonValues)
          objectSchema.patternProperties.map(_.flatMap(collectCommonValues)).getOrElse(m1)
        case oneOfSchema: OneOfAnyOfSchema =>
          oneOfSchema.variants.flatMap(collectCommonValues)
        case allOfSchema: AllOfSchema =>
          allOfSchema.partials.flatMap(collectCommonValues)
        case arraySchema: ArraySchema =>
          arraySchema.items.map(_.flatMap(collectCommonValues)).getOrElse(Seq.empty)
        case _ => Seq.empty
      })

  def quoted(s: String): String = "\"\"\"" + s + "\"\"\""
}

case class ScalaCodeGeneratorContext(
  options: ScalaCodeGeneratorOptions,
  uniqueKey: Option[(String, String)],
  keys: Seq[(String, String)],
  commonValues: Map[String, String]) {

  val renderGenerators: Boolean = options.features.contains(JsonSchema2ScalaFeature.Generator)
  val renderValidators: Boolean = options.features.contains(JsonSchema2ScalaFeature.Validator)
  val renderBuilders: Boolean = options.features.contains(JsonSchema2ScalaFeature.Builder)
  val renderPlayJson: Boolean = options.features.contains(JsonSchema2ScalaFeature.PlayJson)
  val renderSanitizer: Boolean = options.features.contains(JsonSchema2ScalaFeature.Sanitizer)
  val packageName: String = options.packageName

  def toOption(b: Boolean): Option[Unit] = if (b) Some(()) else None

  val generatorsOpt: Option[Unit] = toOption(options.features.contains(JsonSchema2ScalaFeature.Generator))
  val validatorsOpt: Option[Unit] = toOption(options.features.contains(JsonSchema2ScalaFeature.Validator))
  val playJsonOpt: Option[Unit] = toOption(options.features.contains(JsonSchema2ScalaFeature.PlayJson))

  val commonValuesObjectName = "CommonValues"

  def commonReference(s: String): String = commonValues.get(s).map(n => s"$commonValuesObjectName.$n").getOrElse(s)
}
