package uk.gov.hmrc.jsonschema2scala

import java.security.MessageDigest

object SHA256 {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  final def hashOf(string: String, numberOfBytes: Int = 32): String = {
    val bytes = string.getBytes("UTF-8")
    digest
      .digest(bytes)
      .take(numberOfBytes)
      .map(b => Integer.toHexString(0xff & b))
      .mkString
  }

}
