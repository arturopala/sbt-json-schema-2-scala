package uk.gov.hmrc.jsonschema2scala.utils

import play.api.libs.json.{JsArray, JsObject, JsValue}

object JsonUtils {

  def transformObjectFields(json: JsObject)(fx: PartialFunction[(String, JsValue), (String, JsValue)]): JsObject =
    JsObject(json.fields.map {
      case (name, value) => if (fx.isDefinedAt((name, value))) fx((name, value)) else (name, value)
    })

  def filterObjectFields[T](json: JsObject)(fx: String => Boolean): JsObject =
    JsObject(json.fields.filter { case (name, _) => fx(name) })

  def visitObjectFields[T](json: JsObject)(fx: PartialFunction[(String, JsValue), Seq[T]]): Seq[T] =
    json.fields.flatMap { case (name, value) => if (fx.isDefinedAt((name, value))) fx((name, value)) else Seq.empty }

  def transformArrayValues(json: JsArray)(fx: PartialFunction[(Int, JsValue), JsValue]): JsArray =
    JsArray(json.value.zipWithIndex.map {
      case (value, index) => if (fx.isDefinedAt((index, value))) fx((index, value)) else value
    })

  def visitArrayValues[T](json: JsArray)(fx: PartialFunction[(Int, JsValue), Seq[T]]): Seq[T] =
    json.value.zipWithIndex.flatMap {
      case (value, index) => if (fx.isDefinedAt((index, value))) fx((index, value)) else Seq.empty
    }

  def deepMerge(left: JsValue, right: JsValue): JsValue = (left, right) match {
    case (l: JsObject, r: JsObject) =>
      JsObject(l.fields ++ r.fields.map {
        case (key, vr) =>
          (
            key,
            l.fields
              .find(_._1 == key)
              .map(_._2)
              .map(vl => deepMerge(vl, vr))
              .getOrElse(vr))
      })
    case (l: JsArray, r: JsArray) => JsArray((l.value ++ r.value).distinct)
    case (l: JsArray, r: JsValue) => JsArray((l.value :+ r).distinct)
    case (l: JsValue, r: JsArray) => JsArray((l +: r.value).distinct)
    case (l, r)                   => if (l == r) l else JsArray(Seq(l, r))
  }

}
