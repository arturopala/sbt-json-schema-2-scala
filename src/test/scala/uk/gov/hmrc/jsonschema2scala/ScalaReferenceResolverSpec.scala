package uk.gov.hmrc.jsonschema2scala

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.jsonschema2scala.schema.SchemaReferenceResolver.{pathToReference, toJsonPointer}

class ScalaReferenceResolverSpec extends WordSpec with Matchers {

  "ScalaReferenceResolver" should {
    "convert path to reference" in {
      pathToReference(Nil) shouldBe ""
      pathToReference(List("#", "http://test.json")) shouldBe "http://test.json"
      pathToReference(List("foo", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/foo"
      pathToReference(List("foo2", "foo1", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/foo1/foo2"
      pathToReference(List("foo2", "", "foo1", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/foo1/foo2"
      pathToReference(List("foo2", "", "foo1", "", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/foo1/foo2"
      pathToReference(List("foo2", "^_", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/%5E_/foo2"
      pathToReference(List("@foo2", "@foo[1]", "#", "http://test.json/bar")) shouldBe "http://test.json/bar#/%40foo%5B1%5D/%40foo2"
    }

    "convert reference to JSON pointer" in {
      toJsonPointer("foo/bar") shouldBe Nil
      toJsonPointer("/foo/bar") shouldBe Nil
      toJsonPointer("foo") shouldBe Nil
      toJsonPointer("/foo") shouldBe Nil
      toJsonPointer("#/foo") shouldBe List("foo")
      toJsonPointer("http://test/bar#/foo") shouldBe List("foo")
      toJsonPointer("http://test/bar/#/foo") shouldBe List("foo")
      toJsonPointer("#/foo/bar") shouldBe List("foo", "bar")
      toJsonPointer("#/foo/0/bar") shouldBe List("foo", "0", "bar")
      toJsonPointer("#/foo/0/%5E_") shouldBe List("foo", "0", "^_")
      toJsonPointer("#/%40foo%5B1%5D/%40foo2") shouldBe List("@foo[1]", "@foo2")
      toJsonPointer("#/%40foo%5B1%5D/%40foo2") shouldBe List("@foo[1]", "@foo2")
      toJsonPointer("https://test/0/foo/#/%40foo%5B1%5D/%40foo2") shouldBe List("@foo[1]", "@foo2")
    }
  }

}
