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

  def deepMerge(existingObject: JsObject, otherObject: JsObject): JsObject = {
    val result = existingObject.fields ++ otherObject.fields.map {
      case (otherKey, otherValue) =>
        val maybeExistingValue =
          existingObject.fields.find(_._1 == otherKey).map(_._2)

        val newValue = (maybeExistingValue, otherValue) match {
          case (Some(e: JsObject), o: JsObject) => deepMerge(e, o)
          case (Some(e: JsArray), o: JsArray)   => JsArray((e.value ++ o.value).distinct)
          case (Some(e: JsArray), o: JsValue)   => JsArray((e.value :+ o).distinct)
          case (Some(e: JsValue), o: JsArray)   => JsArray((e +: o.value).distinct)
          case (Some(e: JsValue), o: JsValue)   => if (e == o) e else JsArray(Seq(e, o))
          case _                                => otherValue
        }

        (otherKey, newValue)
    }
    JsObject(result)
  }

}
