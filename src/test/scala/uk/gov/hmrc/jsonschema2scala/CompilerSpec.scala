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

package uk.gov.hmrc.jsonschema2scala

import java.nio.file.Paths

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class CompilerSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  implicit val compiler = Compiler()

  override def afterAll(): Unit =
    compiler.cleanup()

  "Compiler" should {
    "compile single scala object definition" in {
      val code: String =
        """
          |object A
          """.stripMargin

      compiler
        .compile(List((Paths.get("A.scala"), code)))
        .fold(
          e => throw new Exception(e.mkString(",")),
          cl => cl.loadClass("A")
        )

    }

    "compile single scala class definition" in {
      val code: String =
        """
          |trait B
          |
          |class C(val i: Int) extends B {
          |    def show: String = i.toString
          |}
        """.stripMargin

      compiler
        .compile(List((Paths.get("BC.scala"), code)))
        .fold(
          e => throw new Exception(e.mkString(",")),
          cl => {
            cl.loadClass("B")
            cl.loadClass("C")
          }
        )
    }

    "compile multiple scala definitions" in {
      val square: String =
        """
          |case class Square(s: Int) {
          |   def area: Int = s*s
          |}
        """.stripMargin

      val board: String =
        """
          |class Board(cells: List[Square]) {
          |   def area: Int = cells.map(_.area).sum
          |}
        """.stripMargin

      val test: String =
        """
          |object Test {
          |
          |  val s1 = Square(5)
          |  val s2 = Square(4)
          |  val s3 = Square(3)
          |
          |  val b = new Board(List(s1,s2,s3))
          |
          |  println(b.area)
          |
          |}
        """.stripMargin

      compiler
        .compile(
          List((Paths.get("Square.scala"), square), (Paths.get("Board.scala"), board), (Paths.get("Test.scala"), test)))
        .fold(
          e => throw new Exception(e.mkString(",")),
          cl => {
            cl.loadClass("Square")
            cl.loadClass("Board")
            cl.loadClass("Test$")
          }
        )
    }

  }
}
