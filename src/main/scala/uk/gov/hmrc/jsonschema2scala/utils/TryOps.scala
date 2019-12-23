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
