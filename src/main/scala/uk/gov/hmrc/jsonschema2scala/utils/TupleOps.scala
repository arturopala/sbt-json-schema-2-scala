package uk.gov.hmrc.jsonschema2scala.utils

object TupleOps {

  implicit class PairExtensions[A, B](pair: (A, B)) {

    def mapFirst[C](f: A => C): (C, B) = (f(pair._1), pair._2)

    def mapSecond[C](f: B => C): (A, C) = (pair._1, f(pair._2))
  }

}
