package org.mulesoft.scheletor

import org.scalatest.{FunSuite, Matchers}

trait PointerTest extends FunSuite with Matchers {

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
}
