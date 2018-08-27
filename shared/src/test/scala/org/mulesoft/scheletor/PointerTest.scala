package org.mulesoft.scheletor

import org.scalatest.{FunSuite, Matchers, OptionValues}
import org.yaml.model.YDocument.{list, obj}
import Primitives._
import ObjLike._
import org.yaml.model.YNode
import syaml._

trait PointerTest extends FunSuite with Matchers with OptionValues {

  test("Simple Path") {
    val p = Pointer / "a" / 0 / "b"
    p.toString shouldBe "/a/0/b"
    p shouldBe Pointer("/a/0/b")
    p shouldBe Pointer("a", 0, "b")

    p ++ Pointer("/c/1") shouldBe Pointer("/a/0/b/c/1")

    p.toList shouldBe List(Pointer.Prop("a"), Pointer.Idx(0), Pointer.Prop("b"))
  }
  test("Escaped Path") {
    val p = Pointer / "a/b" / 0 / "b~c"
    p.toString shouldBe "/a~1b/0/b~0c"
    p shouldBe Pointer("/a~1b/0/b~0c")
    p shouldBe Pointer("a/b", 0, "b~c")
    p.toList shouldBe List(Pointer.Prop("a/b"), Pointer.Idx(0), Pointer.Prop("b~c"))
  }
  test("Extraction") {

    val doc = obj(a = "x",
                  b = 10,
                  c = true,
                  d = list(
                      10,
                      obj(
                          a = "xx",
                          b = "yyy"
                      ),
                      "yyyy"
                  ))

    Pointer.empty.extract("Hello").value shouldBe "Hello"

    doc.extract(Pointer("/a")).value.as[String] shouldBe "x"

    doc.extract("/d/0").value.as[Int] shouldBe 10

    doc.extract("/d/1/a").value.as[String] shouldBe "xx"

    doc.extract("/d/0/a") shouldBe None
    doc.extract("/d").value.as[Seq[YNode]].head.as[Int] shouldBe 10
  }

}
