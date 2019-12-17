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

import uk.gov.hmrc.jsonschema2scala.schema.{ArraySchema, ObjectSchema, OneOfSchema, Schema, StringSchema}

object ScalaCodeRendererContext extends CodeRendererUtils {

  def apply(schema: Schema, options: ScalaCodeRendererOptions): ScalaCodeRendererContext = {

    val uniqueKey = findUniqueKey(schema)
    val keys = findKeys(schema)

    val externalizedStrings = mapCommonVals(schema, Map.empty.withDefaultValue(Nil))
      .mapValues(list => {
        list.map(_.replaceAll("\\d", "")).distinct.minBy(_.length)
      })
      .groupBy { case (_, v) => v }
      .mapValues(m => if (m.size <= 1) m else m.toSeq.zipWithIndex.map { case ((k, v), i) => (k, v + i) }.toMap)
      .foldLeft[Map[String, String]](Map())((a, v) => a ++ v._2)

    ScalaCodeRendererContext(options, uniqueKey, keys, externalizedStrings)
  }

  private def findUniqueKey(definition: Schema, path: List[Schema] = Nil): Option[(String, String)] =
    definition match {
      case s: StringSchema => if (s.isUniqueKey) Some((accessorFor(s :: path), s.name)) else None
      case o: ObjectSchema =>
        o.properties.foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, o :: path)))
      case _ => None
    }

  private def findKeys(definition: Schema, path: List[Schema] = Nil): Seq[(String, String)] =
    definition match {
      case s: StringSchema => if (s.isKey) Seq((accessorFor(s :: path), s.name)) else Seq.empty
      case o: ObjectSchema =>
        o.properties.map(findKeys(_, o :: path)).reduce(_ ++ _)
      case _ => Seq.empty
    }

  private def accessorFor(path: List[Schema], nested: String = "", option: Boolean = false): String = path match {
    case (o: ObjectSchema) :: xs =>
      val prefix =
        if (o.name.isEmpty) ""
        else if (o.mandatory) s"${ScalaTypeNameProvider.safe(o.name)}."
        else s"${o.name}.${if (option) "flatMap" else "map"}(_."
      val suffix = if (o.name.isEmpty) "" else if (!o.mandatory) ")" else ""
      accessorFor(xs, prefix + nested + suffix, !o.mandatory || option)
    case (s: Schema) :: xs =>
      accessorFor(xs, s.name, !s.mandatory)
    case Nil => if (option) nested else s"Option($nested)"
  }

  private def externalizePattern(s: StringSchema, map: Map[String, List[String]]): Map[String, List[String]] =
    s.pattern
      .map(p => {
        val key = quoted(p)
        map
          .get(key)
          .map(list => map.updated(key, s"${s.name}Pattern" :: list))
          .getOrElse(map.+(key -> List(s"${s.name}Pattern")))
      })
      .getOrElse(map)

  private def externalizeEnum(s: StringSchema, map: Map[String, List[String]]): Map[String, List[String]] =
    s.enum
      .map(e => {
        val key = s"""Seq(${e.mkString("\"", "\",\"", "\"")})"""
        map
          .get(key)
          .map(list => map.updated(key, s"${s.name}Enum" :: list))
          .getOrElse(map.+(key -> List(s"${s.name}Enum")))
      })
      .getOrElse(map)

  private def mapCommonVals(definition: Schema, map: Map[String, List[String]]): Map[String, List[String]] =
    definition match {
      case s: StringSchema => externalizePattern(s, map) ++ externalizeEnum(s, map)
      case o: ObjectSchema =>
        o.properties.foldLeft(map)((m, p) => mapCommonVals(p, m))
      case o: OneOfSchema =>
        o.variants.foldLeft(map)((m, p) => mapCommonVals(p, m))
      case a: ArraySchema =>
        mapCommonVals(a.item, map)
      case _ => map
    }
}

case class ScalaCodeRendererContext(
  options: ScalaCodeRendererOptions,
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
