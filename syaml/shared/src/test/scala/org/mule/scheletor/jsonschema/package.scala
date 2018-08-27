package org.mule.scheletor

import org.scalatest.Matchers

import scala.reflect.ClassTag

package object jsonschema {
  implicit class SchemaConv(val s: Schema)  extends Matchers {
    def as[T: ClassTag]: T = {
      s shouldBe a[T]
      s.asInstanceOf[T]
    }
  }
}
