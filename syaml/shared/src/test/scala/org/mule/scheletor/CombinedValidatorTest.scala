package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.SchemaBuilder._
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YNode
import org.mule.scheletor.syaml._
import Primitives._

trait CombinedValidatorTest extends FunSuite with Matchers {
  private val multipleOf10 = integerSchema.multipleOf(10).build
  private val multipleOf3  = integerSchema.multipleOf(3).build

  private def validate(node: YNode)(implicit schema: Schema) = Validator.validate(schema, node)

  private def listOf(errors: ErrorType*) = errors.map(ValidationError("/", _)).toList

  test("All Of") {
    implicit val schema: Schema = allOfSchema().schema(multipleOf10).schema(multipleOf3).build

    validate(60) shouldBe empty
    validate(15) shouldBe listOf(NotMultipleOf(15, 10))
    validate(11) shouldBe listOf(NotMultipleOf(11, 10), NotMultipleOf(11, 3))
  }

  test("One Of") {
    implicit val schema: Schema = oneOfSchema.schema(multipleOf10).schema(multipleOf3).build

    validate(15) shouldBe empty
    validate(6) shouldBe empty
    validate(60) shouldBe listOf(ManyMatched(2))
    validate(11) shouldBe listOf(NoneMatched)
  }

  test("Any Of") {
    implicit val schema: Schema = anyOfSchema().schema(multipleOf10).schema(multipleOf3).build

    validate(15) shouldBe empty
    validate(6) shouldBe empty
    validate(60) shouldBe empty
    validate(11) shouldBe listOf(NoneMatched)
  }

  test("Empty Conditionals") {
    validate("x")(ifSchema(None, Some(BooleanSchema)).build) shouldBe empty
    validate("x")(ifSchema(Some(BooleanSchema)).build) shouldBe empty
    validate("x")(ifSchema(None, None, Some(BooleanSchema)).build) shouldBe empty
  }

  test("If Then Conditionals") {
    implicit val schema: Schema = ifSchema(Some(StringSchema), Some(stringSchema.minLength(5).build)).build

    validate("abcdefg") shouldBe empty
    validate(10) shouldBe empty
    validate("abc") shouldBe listOf(MinLength(5, 3))
  }

  test("If Else Conditionals") {
    implicit val schema: Schema =
      ifSchema(Some(BooleanSchema), elseSchema = Some(stringSchema.minLength(5).build)).build

    validate(true) shouldBe empty
    validate("abcdefgh") shouldBe empty
    validate("abc") shouldBe listOf(MinLength(5, 3))
    validate(10) shouldBe listOf(NotString)
  }

  test("If Then Else Conditionals") {
    implicit val schema: Schema =
      ifSchema(Some(NumberSchema), Some(numberSchema.maximum(10.0)), Some(stringSchema.minLength(5)))

    validate(5) shouldBe empty
    validate("abcdefgh") shouldBe empty
    validate("abc") shouldBe listOf(MinLength(5, 3))
    validate(20) shouldBe listOf(OutOfRange(20.0, "greater than", 10.0))
  }

}
