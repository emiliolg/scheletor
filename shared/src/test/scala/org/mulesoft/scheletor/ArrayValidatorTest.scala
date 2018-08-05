package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.SchemaBuilder._
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YDocument.{list, obj}
import org.yaml.model.{YDocument, YNode}

trait ArrayValidatorTest extends FunSuite with Matchers {
  private def validate(doc: YDocument)(implicit schema: Schema)   = Validator.validate(schema, doc.node)
  private def listOf(errors: ErrorType*) = errors.map(ValidationError("/", _)).toList
  private def error(location: String, errorType: ErrorType) = ValidationError(Pointer(location), errorType)

  test("Min Max Items") {
    implicit val schema: ArraySchema = arraySchema.minItems(2).maxItems(3).build

    validate(list("x", "y", "z")) shouldBe empty

    validate(list("x")) shouldBe listOf(MinItems(2, 1))

    validate(list("x", 10, true, "d")) shouldBe listOf(MaxItems(3, 4))
  }
  test("No Items Schema") {
    validate(list("a", 2, 3))(arraySchema.build)
  }
  test("Contains") {
    implicit val schema: ArraySchema = arraySchema.contains(NullSchema).build
    validate(list(0)) shouldBe listOf(DoesNotContain)
    validate(list()) shouldBe listOf(DoesNotContain)

  }
  test("Unique") {
    implicit val schema: ArraySchema = arraySchema.uniqueItems(true).build

    validate(list(0, 0, 1)) shouldBe List(error("/1", NotUnique))

    validate(list(obj(a = 0), obj(b = 0), obj(a = 0))) shouldBe List(error("/2", NotUnique))

    validate(list(obj(a = 0), obj(a = 1), obj(a = 2))) shouldBe empty

    validate(list(obj(a = "b"), "{\"a\":\"b\"}")) shouldBe empty

    validate(list(list("foo"), list("goo"), list("foo"))) shouldBe List(error("/2", NotUnique))
  }
  test("Boolean All Items schema") {
    implicit val schema: ArraySchema = arraySchema.allItems(BooleanSchema).build
    validate(list(true, false, "true")) shouldBe List(error("/2", NotBoolean))
  }
  test("Tuple like") {
      implicit val schema: ArraySchema = arraySchema.item(BooleanSchema).additionalItems(false).build

      validate(list(true)) shouldBe empty
      validate(list("true")) shouldBe List(error("/0", NotBoolean))
      validate(list(true, false)) shouldBe listOf(MaxItems(1, 2))

      validate(list(YNode.Null))(arraySchema.item(EmptySchema).item(BooleanSchema).build) shouldBe empty
  }
  test("Array not specified") {
      implicit val schema: ArraySchema = arrayLikeSchema.uniqueItems(true).build

      Validator.validate(schema, "x") shouldBe empty
      validate(list( obj( a = "b"), "x")) shouldBe empty

      validate(list( obj( a = "b"), "x", obj(a = "b"))) shouldBe List(error("/2", NotUnique))

  }

    test("Additional Item Schema") {
        implicit val schema: ArraySchema = arrayLikeSchema.item(BooleanSchema).additionalItems(NullSchema).build

        validate(list(true, YNode.Null, YNode.Null)) shouldBe empty
        validate(list(true, YNode.Null, false)) shouldBe List(error("/2", NotNull))

    }

}
