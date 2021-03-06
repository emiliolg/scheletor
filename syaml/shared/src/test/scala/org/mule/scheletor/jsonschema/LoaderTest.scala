package org.mule.scheletor.jsonschema

import org.mule.scheletor.syaml._
import org.scalatest.{FunSuite, Matchers, OptionValues}
import org.yaml.model.YDocument.{list, obj}
import org.yaml.model.{YDocument, YNode}

import scala.reflect.ClassTag

trait LoaderTest extends FunSuite with Matchers  with OptionValues {

  val simpleTypes: YNode = list(
      obj(`type` = "integer"),
      obj(`type` = "string"),
      obj(`type` = "boolean"),
      true,
      false
  )

  def load[T: ClassTag](doc: YNode, specVersion: SpecVersion = Draft7): T =
    JsonSchemaLoader.load(YDocument(doc), specVersion).as[T]

}
