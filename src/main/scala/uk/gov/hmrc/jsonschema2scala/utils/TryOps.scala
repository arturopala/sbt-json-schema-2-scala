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

import scala.util.{Failure, Success, Try}

object TryOps {

  implicit class TryFold[A](t: Try[A]) {

    def toEither: Either[Throwable, A] =
      t.transform[Either[Throwable, A]](
          a => Success(Right.apply[Throwable, A](a)),
          e => Success(Left.apply[Throwable, A](e))
        )
        .get

    def logError(log: Throwable => Unit): Try[A] =
      t.transform[A](
        a => Success(a),
        e => {
          log(e)
          Failure(e)
        }
      )
  }

}
