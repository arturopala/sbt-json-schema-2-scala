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

package uk.gov.hmrc.jsonschema2scala.generator

object TextUtils {

  final def splitAndNormalize(text: String, length: Int): List[String] = {
    val betterLength = {
      val equal = text.length / (text.length / length + 1)
      val diff = Math.abs(equal - length)
      if (diff > 2 && diff < (length / 3)) equal else length
    }
    text
      .split('\n')
      .foldLeft(List.empty[String])(
        (acc, v) =>
          if (v.trim.nonEmpty) v :: acc
          else
            acc match {
              case Nil                       => v :: Nil
              case x :: xs if x.trim.isEmpty => xs
              case _                         => v :: acc
          })
      .reverse
      .flatMap(splitAround(_, betterLength)) match {
      case Nil     => Nil
      case x :: xs => if (xs.isEmpty || x.trim.isEmpty) x :: xs else "" :: x :: xs
    }
  }

  final def splitAround(string: String, length: Int): List[String] =
    if (string.length < length) List(string)
    else {
      val p = findSplitPosition(string, length)
      if (p < 0 || (string.length - p) < (length / 5)) List(string)
      else {
        val slice = string.substring(0, p + 1)
        val next = {
          val s = string.substring(p + 1)
          if ((slice.endsWith(",") || slice.endsWith(".")) && s.startsWith(" ")) s.substring(1) else s
        }
        slice :: splitAround(next, length)
      }
    }

  final def findSplitPosition(string: String, length: Int): Int = {
    val positions = List(
      (string.indexOf('.', Math.max(length - (length / 5), 0)), 0.5, 0),
      (string.indexOf('.', length), 0.5, 1),
      (string.indexOf(',', Math.max(length - (length / 5), 0)), 1.0, 4),
      (string.indexOf(',', length), 1.0, 5),
      (string.indexOf(' ', Math.max(length - (length / 5), 0)), 2.0, 8),
      (string.indexOf(' ', length), 2.0, 9)
    ).filter(_._1 >= 0).map {
      case (pos, weight, offset) =>
        (pos, Math.abs(length - pos) * weight + offset)
    }
    positions match {
      case Nil      => -1
      case nonEmpty => nonEmpty.minBy(_._2)._1
    }
  }
}