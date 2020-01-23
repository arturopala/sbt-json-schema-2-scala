package uk.gov.hmrc.jsonschema2scala.utils

object SeqOps {

  object isEmpty {
    def unapply(seq: Seq[_]): Boolean = seq.isEmpty
  }

  object hasSingleItem {
    def unapply[T](seq: Seq[T]): Option[T] =
      if (seq.size == 1) Some(seq.head) else None
  }

}
