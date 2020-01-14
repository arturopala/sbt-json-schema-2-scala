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

package uk.gov.hmrc.jsonschema2scala.utils

object EitherX {

  def combine[L, R](combineLeft: (L, L) => L, combineRight: (R, R) => R)(
    a: Either[L, R],
    b: Either[L, R]): Either[L, R] =
    (a, b) match {
      case (Right(a), Right(b)) => Right(combineRight(a, b))
      case (Left(a), Left(b))   => Left(combineLeft(a, b))
      case (_, b)               => b
    }
}
