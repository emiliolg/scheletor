package org.mule.scheletor

import org.mule.scheletor.jsonschema.{Draft7, JsonSchemaLoader, SpecVersion}
import org.mule.scheletor.syaml._
import org.mulesoft.common.io.FileSystem
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model._
import org.yaml.parser.JsonParser

/**
  * Golden based test
  */
trait JsonTestSuite extends FunSuite with Matchers {
  def fs: FileSystem

  private val exceptions = Seq("refRemote.json", "ref.json", "definitions.json")

  def draft: SpecVersion  = Draft7
  private val baseTestDir = fs syncFile "JSON-Schema-Test-Suite/tests"
  private val testDir     = baseTestDir / draft.id.toLowerCase

  private val singleTest      = System.getProperty("test", "")
  private val testDescription = System.getProperty("description", "")
  private val files           = if (singleTest.isEmpty) testDir.list.diff(exceptions).sorted else Array(singleTest + ".json")

  for {
    file <- files
    fullFile = testDir / file
    if fullFile.isFile
    doc = JsonParser((testDir / file).read()).documents()(0)
    testNode <- doc.as[Seq[YNode]]
    testDoc = testNode.obj
  } {
    val testName = s"Json (${draft.id}): $file - ${testDoc.description}"

    test(testName) {
      val schema = JsonSchemaLoader.load(asNode(testDoc.schema), draft)
      for {
        t <- testDoc.tests.as[Seq[YNode]]
        to    = t.obj
        data  = asNode(to.data)
        valid = to.valid.as[Boolean]
        descr = t.obj.description.as[String]
        if testDescription.isEmpty || testDescription == descr
      } {

        val errors = Validator.validate(schema, data)
        if (errors.isEmpty != valid) fail(descr)
      }
    }
  }

  def asNode(obj: YObj): YNode = obj match {
    case YSuccess(node) => node
    case YFail(err) =>
      fail(err.error)
  }

}
