package org.mulesoft.scheletor.jsonschema

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YNode
import org.yaml.model.YDocument.{list, obj}

import scala.reflect.ClassTag

trait LoaderTest extends FunSuite with Matchers {

  val simpleTypes: YNode = list(
      obj(`type` = "integer"),
      obj(`type` = "string"),
      obj(`type` = "boolean")
  )

  def load[T: ClassTag](doc: YNode, specVersion: SpecVersion = Draft7): T = {
    val s = JsonSchemaLoader.load(doc, specVersion)
    s shouldBe a[T]
    s.asInstanceOf[T]
  }

}
