package uk.gov.hmrc.jsonschema2scala

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
