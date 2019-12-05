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

object ScalaCodeRendererContext extends CodeRendererUtils {

  def apply(definition: Definition, options: JsonSchema2ScalaOptions): ScalaCodeRendererContext = {

    val uniqueKey = findUniqueKey(definition)
    val keys = findKeys(definition)

    val externalizedStrings = mapCommonVals(definition, Map.empty.withDefaultValue(Nil))
      .mapValues(list => {
        list.map(_.replaceAll("\\d", "")).distinct.minBy(_.length)
      })
      .groupBy { case (_, v) => v }
      .mapValues(m => if (m.size <= 1) m else m.toSeq.zipWithIndex.map { case ((k, v), i) => (k, v + i) }.toMap)
      .foldLeft[Map[String, String]](Map())((a, v) => a ++ v._2)

    ScalaCodeRendererContext(options, uniqueKey, keys, externalizedStrings)
  }

  private def findUniqueKey(definition: Definition, path: List[Definition] = Nil): Option[(String, String)] =
    definition match {
      case s: StringDefinition => if (s.isUniqueKey) Some((accessorFor(s :: path), s.name)) else None
      case o: ObjectDefinition =>
        o.properties.foldLeft[Option[(String, String)]](None)((a, p) => a.orElse(findUniqueKey(p, o :: path)))
      case _ => None
    }

  private def findKeys(definition: Definition, path: List[Definition] = Nil): Seq[(String, String)] =
    definition match {
      case s: StringDefinition => if (s.isKey) Seq((accessorFor(s :: path), s.name)) else Seq.empty
      case o: ObjectDefinition =>
        o.properties.map(findKeys(_, o :: path)).reduce(_ ++ _)
      case _ => Seq.empty
    }

  private def accessorFor(path: List[Definition], nested: String = "", option: Boolean = false): String = path match {
    case (o: ObjectDefinition) :: xs =>
      val prefix =
        if (o.name.isEmpty) ""
        else if (o.isMandatory) s"${safeName(o.name)}."
        else s"${o.name}.${if (option) "flatMap" else "map"}(_."
      val suffix = if (o.name.isEmpty) "" else if (!o.isMandatory) ")" else ""
      accessorFor(xs, prefix + nested + suffix, !o.isMandatory || option)
    case (s: Definition) :: xs =>
      accessorFor(xs, s.name, !s.isMandatory)
    case Nil => if (option) nested else s"Option($nested)"
  }

  private def externalizePattern(s: StringDefinition, map: Map[String, List[String]]): Map[String, List[String]] =
    s.pattern
      .map(p => {
        val key = quoted(p)
        map
          .get(key)
          .map(list => map.updated(key, s"${s.name}Pattern" :: list))
          .getOrElse(map.+(key -> List(s"${s.name}Pattern")))
      })
      .getOrElse(map)

  private def externalizeEnum(s: StringDefinition, map: Map[String, List[String]]): Map[String, List[String]] =
    s.enum
      .map(e => {
        val key = s"""Seq(${e.mkString("\"", "\",\"", "\"")})"""
        map
          .get(key)
          .map(list => map.updated(key, s"${s.name}Enum" :: list))
          .getOrElse(map.+(key -> List(s"${s.name}Enum")))
      })
      .getOrElse(map)

  private def mapCommonVals(definition: Definition, map: Map[String, List[String]]): Map[String, List[String]] =
    definition match {
      case s: StringDefinition => externalizePattern(s, map) ++ externalizeEnum(s, map)
      case o: ObjectDefinition =>
        o.properties.foldLeft(map)((m, p) => mapCommonVals(p, m))
      case o: OneOfDefinition =>
        o.variants.foldLeft(map)((m, p) => mapCommonVals(p, m))
      case a: ArrayDefinition =>
        mapCommonVals(a.item, map)
      case _ => map
    }
}

case class ScalaCodeRendererContext(
  options: JsonSchema2ScalaOptions,
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

  def commonReference(s: String): String = commonVals.get(s).map(n => s"Common.$n").getOrElse(s)
}
