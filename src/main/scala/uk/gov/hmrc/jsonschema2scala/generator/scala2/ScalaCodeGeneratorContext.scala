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

package uk.gov.hmrc.jsonschema2scala.generator.scala2

import uk.gov.hmrc.jsonschema2scala.schema._
import uk.gov.hmrc.jsonschema2scala.typer.ScalaTypeNameProvider

object ScalaCodeGeneratorContext {

  def apply(schema: Schema, options: ScalaCodeGeneratorOptions): ScalaCodeGeneratorContext = {

    val uniqueKey = findUniqueKey(schema)
    val keys = findKeys(schema)

    val externalizedStrings = mapCommonVals(schema)
      .groupBy(_._1)
      .mapValues(_.map(_._2).map(_.replaceAll("\\d", "")).distinct.minBy(_.length))
      .groupBy(_._2)
      .mapValues(m => if (m.size <= 1) m else m.toSeq.zipWithIndex.map { case ((k, v), i) => (k, v + i) }.toMap)
      .foldLeft[Map[String, String]](Map())((a, v) => a ++ v._2)

    ScalaCodeGeneratorContext(options, uniqueKey, keys, externalizedStrings)
  }

  private def findUniqueKey(schema: Schema, path: List[Schema] = Nil): Option[(String, String)] =
    (schema match {
      case s: StringSchema => if (s.hasCustom("x_uniqueKey")) Some((accessorFor(s :: path), s.name)) else None
      case o: ObjectSchema =>
        o.properties.foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, o :: path)))
      case _ => None
    }).orElse(schema.definitions
      .foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, path))))

  private def findKeys(schema: Schema, path: List[Schema] = Nil): Seq[(String, String)] =
    (schema match {
      case s: StringSchema => if (s.hasCustom("x_key")) Seq((accessorFor(s :: path), s.name)) else Seq.empty
      case o: ObjectSchema =>
        o.properties.flatMap(findKeys(_, o :: path))
      case _ => Seq.empty
    }) ++ schema.definitions.flatMap(s => findKeys(s, s :: path))

  private def accessorFor(path: List[Schema], nested: String = "", option: Boolean = false): String = path match {
    case (o: ObjectSchema) :: xs =>
      val prefix =
        if (o.name.isEmpty) ""
        else if (o.required) s"${ScalaTypeNameProvider.toIdentifier(o.name)}."
        else s"${o.name}.${if (option) "flatMap" else "map"}(_."
      val suffix = if (o.name.isEmpty) "" else if (!o.required) ")" else ""
      accessorFor(xs, prefix + nested + suffix, !o.required || option)
    case (s: Schema) :: xs =>
      accessorFor(xs, s.name, !s.required)
    case Nil => if (option) nested else s"Option($nested)"
  }

  private def externalizePattern(schema: StringSchema): Seq[(String, String)] =
    schema.pattern
      .map(p => {
        val key = quoted(p)
        (key, s"${schema.name}Pattern")
      })
      .toSeq

  private def externalizeEnum(schema: StringSchema): Seq[(String, String)] =
    schema.enum
      .map(e => (s"""Seq(${e.mkString("\"", "\",\"", "\"")})""", s"${schema.name}Enum"))
      .toSeq

  private def mapCommonVals(schema: Schema): Seq[(String, String)] =
    schema.definitions.flatMap(mapCommonVals) ++
      (schema match {
        case stringSchema: StringSchema =>
          externalizePattern(stringSchema) ++ externalizeEnum(stringSchema)
        case objectSchema: ObjectSchema =>
          val m1 = objectSchema.properties.flatMap(mapCommonVals)
          objectSchema.patternProperties.map(_.flatMap(mapCommonVals)).getOrElse(m1)
        case mapSchema: MapSchema =>
          mapSchema.patternProperties.flatMap(mapCommonVals)
        case oneOfSchema: OneOfAnyOfSchema =>
          oneOfSchema.variants.flatMap(mapCommonVals)
        case arraySchema: ArraySchema =>
          arraySchema.item.map(mapCommonVals).getOrElse(Seq.empty)
        case _ => Seq.empty
      })

  def quoted(s: String): String = "\"\"\"" + s + "\"\"\""
}

case class ScalaCodeGeneratorContext(
  options: ScalaCodeGeneratorOptions,
  uniqueKey: Option[(String, String)],
  keys: Seq[(String, String)],
  commonVals: Map[String, String]) {

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

  def commonReference(s: String): String = commonVals.get(s).map(n => s"Common.$n").getOrElse(s)

}
