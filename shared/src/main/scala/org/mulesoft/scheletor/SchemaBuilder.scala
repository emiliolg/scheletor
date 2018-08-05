package org.mulesoft.scheletor

import org.mulesoft.scheletor.ObjLike._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.matching.Regex

abstract class SchemaBuilder[T <: Schema] {
  private var id           = Option.empty[String]
  private var description  = Option.empty[String]
  private var title        = Option.empty[String]
  private var nullable     = false
  private var readOnly     = false
  private var writeOnly    = false
  private var defaultValue = Option.empty[Any]

  def id(s: String): this.type          = { id = Option(s); this }
  def title(s: String): this.type       = { title = Option(s); this }
  def description(s: String): this.type = { description = Option(s); this }
  def nullable(b: Boolean): this.type   = { nullable = b; this }
  def readOnly(b: Boolean): this.type   = { readOnly = b; this }
  def writeOnly(b: Boolean): this.type  = { writeOnly = b; this }
  def defaultValue(v: Any): this.type   = { defaultValue = Option(v); this }

  def apply(): T = build

  def build: T = doBuild()

  protected def isDefault: Boolean =
    id.isEmpty && description.isEmpty && title.isEmpty && !nullable && !readOnly && !writeOnly

  protected def doBuild(): T

}

object SchemaBuilder {
  implicit def build[T <: Schema](builder: SchemaBuilder[T]): T = builder.build

  final val BooleanSchema = booleanSchema.doBuild()
  final val EmptySchema   = emptySchema.doBuild()
  final val NullSchema    = nullSchema.doBuild()
  final val NumberSchema  = numberSchema.doBuild()
  final val IntegerSchema = integerSchema.doBuild()
  final val StringSchema  = stringSchema.doBuild()

  // Builder for standard types
  def stringSchema                            = new Str()
  def integerSchema                           = new Num(integer = true)
  def numberSchema                            = new Num()
  def booleanSchema                           = new Bool()
  def objectSchema                            = new ObjBuilder()
  def arraySchema                             = new Arr()
  def emptySchema                             = new Empty()
  def nullSchema                              = new Null()
  def anyOfSchema                             = new AnyOf()
  def oneOfSchema                             = new OneOf()
  def notSchema(schema: Schema)               = new Not(schema)
  def constSchema[V: ObjLike](value: V)       = new ConstBuilder(value)
  def enumSchema[V: ObjLike]                  = new Enum[V]()
  def allOfSchema(synthetic: Boolean = false) = new AllOf(synthetic)

  def ifSchema(ifSchema: Schema, thenSchema: Schema = null, elseSchema: Schema = null) =
    new If(ifSchema, thenSchema, elseSchema)

  // Builder when type is not specified but attributes are type like
  def stringLikeSchema = new Str(stringType = false)
  def numberLikeSchema = new Num(numberType = false)
  def objectLikeSchema = new ObjBuilder(objectType = false)
  def arrayLikeSchema  = new Arr(arrayType = false)

  abstract class SchemaImpl(builder: SchemaBuilder[_]) extends Schema {
    val id: String                = builder.id.getOrElse("")
    val description: String       = builder.description.getOrElse("")
    val title: String             = builder.title.getOrElse("")
    val nullable: Boolean         = builder.nullable
    val readOnly: Boolean         = builder.readOnly
    val writeOnly: Boolean        = builder.writeOnly
    val defaultValue: Option[Any] = builder.defaultValue
  }

  abstract class Combined[T <: CombinedSchema](val synthetic: Boolean = false) extends SchemaBuilder[T] {
    private[SchemaBuilder] var schemasBuilder = List.newBuilder[Schema]
    def schema(s: Schema): this.type          = { schemasBuilder += s; this }
  }

  abstract class CombinedImpl[T <: CombinedSchema](builder: Combined[T])
      extends SchemaImpl(builder)
      with CombinedSchema {
    val schemas: List[Schema] = builder.schemasBuilder.result()
  }

  class Bool extends SchemaBuilder[BooleanSchema] {
    override def build: BooleanSchema = if (isDefault) BooleanSchema else doBuild()

    def doBuild(): BooleanSchema = new SchemaImpl(this) with BooleanSchema
  }

  class Null extends SchemaBuilder[NullSchema] {
    override def build: NullSchema = if (isDefault) NullSchema else doBuild()

    def doBuild(): NullSchema = new SchemaImpl(this) with NullSchema
  }

  class Empty() extends SchemaBuilder[EmptySchema] {
    override def build: EmptySchema = if (isDefault) EmptySchema else doBuild()

    def doBuild(): EmptySchema = new SchemaImpl(this) with EmptySchema
  }

  class Not(schema: Schema) extends SchemaBuilder[NotSchema] {
    def doBuild(): NotSchema = new SchemaImpl(this) with NotSchema {
      val innerSchema: Schema = schema
    }
  }

  class If(ifSchema: Schema, thenSchema: Schema, elseSchema: Schema = null) extends SchemaBuilder[IfSchema] {
    self =>
    def doBuild(): IfSchema = new SchemaImpl(this) with IfSchema {
      val ifSchema: Option[Schema]   = Option(self.ifSchema)
      val thenSchema: Option[Schema] = Option(self.thenSchema)
      val elseSchema: Option[Schema] = Option(self.elseSchema)
    }
  }

  class Enum[V: ObjLike] extends SchemaBuilder[EnumSchema[V]] { outer =>
    private val values      = ListBuffer.empty[V]
    private var nullAllowed = false

    def value(v: V): this.type = {
      if (v == null || v.isNull) nullAllowed = true
      if (values.forall(e => !deepEquals(e, v))) values += v
      this
    }

    def doBuild(): EnumSchema[V] = new SchemaImpl(this) with EnumSchema[V] {
      val possibleValues: List[V]                        = values.result()
      override val nullable: Boolean                     = nullAllowed
      override protected def toObjLike(v: V): ObjLike[V] = implicitly[ObjLike[V]]
    }
  }

  class Str(val stringType: Boolean = true) extends SchemaBuilder[StringSchema] {
    private[SchemaBuilder] var minLength: Option[Int]                   = None
    private[SchemaBuilder] var maxLength: Option[Int]                   = None
    private[SchemaBuilder] var pattern: Option[Regex]                   = None
    private[SchemaBuilder] var formatValidator: Option[FormatValidator] = None

    def minLength(v: Int): this.type                   = { minLength = Some(v); this }
    def maxLength(v: Int): this.type                   = { maxLength = Some(v); this }
    def pattern(p: String): this.type                  = { if (p != null && p.nonEmpty) pattern = Some(p.r); this; }
    def formatValidator(f: FormatValidator): this.type = { formatValidator = Some(f); this; }

    def doBuild(): StringSchema = new StrImpl(this)
  }

  class StrImpl(builder: Str) extends SchemaImpl(builder) with StringSchema {
    val string: Boolean                          = builder.stringType
    val minLength: Option[Int]                   = builder.minLength
    val maxLength: Option[Int]                   = builder.maxLength
    val pattern: Option[Regex]                   = builder.pattern
    val formatValidator: Option[FormatValidator] = builder.formatValidator
  }

  class Num(val integer: Boolean = false, val numberType: Boolean = true) extends SchemaBuilder[NumberSchema] {
    private[SchemaBuilder] var minimum_          = Option.empty[Number]
    private[SchemaBuilder] var exclusiveMinimum_ = Option.empty[Number]
    private[SchemaBuilder] var exclusiveMinimumF = false
    private[SchemaBuilder] var maximum_          = Option.empty[Number]
    private[SchemaBuilder] var exclusiveMaximum_ = Option.empty[Number]
    private[SchemaBuilder] var exclusiveMaximumF = false
    private[SchemaBuilder] var multipleOf_       = Option.empty[Number]

    def minimum(v: Long): this.type          = { minimum_ = Option(v); this }
    def maximum(v: Long): this.type          = { maximum_ = Option(v); this }
    def exclusiveMinimum(v: Long): this.type = { exclusiveMinimum_ = Option(v); this }
    def exclusiveMaximum(v: Long): this.type = { exclusiveMaximum_ = Option(v); this }
    def multipleOf(v: Long): this.type       = { multipleOf_ = Option(v); this }

    def minimum(v: Number): this.type          = { minimum_ = Option(v); this }
    def maximum(v: Number): this.type          = { maximum_ = Option(v); this }
    def exclusiveMinimum(v: Number): this.type = { exclusiveMinimum_ = Option(v); this }
    def exclusiveMaximum(v: Number): this.type = { exclusiveMaximum_ = Option(v); this }
    def multipleOf(v: Number): this.type       = { multipleOf_ = Option(v); this }

    def minimum(v: Double): this.type          = { minimum_ = Option(v); this }
    def maximum(v: Double): this.type          = { maximum_ = Option(v); this }
    def exclusiveMinimum(v: Double): this.type = { exclusiveMinimum_ = Option(v); this }
    def exclusiveMaximum(v: Double): this.type = { exclusiveMaximum_ = Option(v); this }
    def multipleOf(v: Double): this.type       = { multipleOf_ = Option(v); this }

    def exclusiveMinimum(b: Boolean): this.type = { exclusiveMinimumF = b; this }
    def exclusiveMaximum(b: Boolean): this.type = { exclusiveMaximumF = b; this }
    def doBuild(): NumberSchema                 = new NumImpl(this)
  }

  class NumImpl(b: Num) extends SchemaImpl(b) with NumberSchema {
    val integer: Boolean                 = b.integer
    val number: Boolean                  = b.numberType
    val minimum: Option[Number]          = if (!b.exclusiveMinimumF) b.minimum_ else None
    val maximum: Option[Number]          = if (!b.exclusiveMaximumF) b.maximum_ else None
    val exclusiveMinimum: Option[Number] = if (b.exclusiveMinimumF) b.minimum_ else b.exclusiveMinimum_
    val exclusiveMaximum: Option[Number] = if (b.exclusiveMaximumF) b.maximum_ else b.exclusiveMaximum_
    val multipleOf: Option[Number]       = b.multipleOf_
  }

  class ObjBuilder(val objectType: Boolean = true) extends SchemaBuilder[ObjectSchema] {
    private[SchemaBuilder] val properties         = Map.newBuilder[String, Schema]
    private[SchemaBuilder] val patternProperties  = Map.newBuilder[Regex, Schema]
    private[SchemaBuilder] val requiredProperties = List.newBuilder[String]
    private[SchemaBuilder] val dependencies       = mutable.Map.empty[String, (Option[Schema], Set[String])]
    private[SchemaBuilder] val defaultDep         = (None, Set.empty[String])

    private[SchemaBuilder] var minProperties              = Option.empty[Int]
    private[SchemaBuilder] var maxProperties              = Option.empty[Int]
    private[SchemaBuilder] var propertyNames_             = Option.empty[Schema]
    private[SchemaBuilder] var additionalPropertiesSchema = Option.empty[Schema]
    private[SchemaBuilder] var allowAdditionalProperties  = true

    def minProperties(v: Int): this.type                    = { minProperties = Some(v); this }
    def maxProperties(v: Int): this.type                    = { maxProperties = Some(v); this }
    def property(name: String): this.type                   = { properties += name -> EmptySchema; this }
    def property(pattern: Regex, schema: Schema): this.type = { patternProperties += pattern -> schema; this }
    def required(name: String, schema: Schema): this.type   = { property(name, schema); required(name) }
    def property(name: String, schema: Schema): this.type   = { properties += name -> schema; this }
    def required(name: String): this.type                   = { requiredProperties += name; this }
    def propertyNames(schema: Schema): this.type            = { propertyNames_ = Option(schema); this }
    def additionalProperties(schema: Schema): this.type     = { additionalPropertiesSchema = Some(schema); this }
    def additionalProperties(allow: Boolean): this.type     = { allowAdditionalProperties = allow; this }

    def dependency(ifProp: String, thenProp: String): this.type = {
      val (o, s) = dependencies.getOrElse(ifProp, defaultDep)
      dependencies += ifProp -> (o, s + thenProp)
      this
    }

    def dependency(ifProp: String, schema: Schema): this.type = {
      val set = dependencies.getOrElse(ifProp, defaultDep)._2
      dependencies += ifProp -> (Option(schema), set)
      this
    }

    def doBuild(): ObjectSchema = new ObjImpl(this)
  }

  class ObjImpl(builder: ObjBuilder) extends SchemaImpl(builder) with ObjectSchema {
    val objectType: Boolean                                      = builder.objectType
    val minProperties: Option[Int]                               = builder.minProperties
    val maxProperties: Option[Int]                               = builder.maxProperties
    val required: List[String]                                   = builder.requiredProperties.result()
    val propertyNames: Option[Schema]                            = builder.propertyNames_
    val properties: Map[String, Schema]                          = builder.properties.result()
    val patternProperties: Map[Regex, Schema]                    = builder.patternProperties.result()
    val additionalPropertiesSchema: Option[Schema]               = builder.additionalPropertiesSchema
    val allowAdditionalProperties: Boolean                       = builder.allowAdditionalProperties
    val dependencies: Map[String, (Option[Schema], Set[String])] = builder.dependencies.toMap
  }

  class ConstBuilder[W: ObjLike](value: W) extends SchemaBuilder[ConstSchema[W]] {
    override protected def doBuild(): ConstSchema[W] = new Const[W](this, value)
  }

  class Const[W: ObjLike](builder: ConstBuilder[W], v: W) extends SchemaImpl(builder) with ConstSchema[W] {
    override val nullable: Boolean                     = toObjLike(value).isNull(value)
    override def value: W                              = v
    override protected def toObjLike(w: W): ObjLike[W] = implicitly[ObjLike[W]]
  }

  class Arr(val arrayType: Boolean = true) extends SchemaBuilder[ArraySchema] {
    private[SchemaBuilder] var minItems              = Option.empty[Int]
    private[SchemaBuilder] var maxItems              = Option.empty[Int]
    private[SchemaBuilder] var uniqueItems           = false
    private[SchemaBuilder] var allItems              = Option.empty[Schema]
    private[SchemaBuilder] var items                 = List.newBuilder[Schema]
    private[SchemaBuilder] var additionalItemsSchema = Option.empty[Schema]
    private[SchemaBuilder] var allowAdditionalItems  = true
    private[SchemaBuilder] var contains              = Option.empty[Schema]

    def allItems(schema: Schema): this.type        = { allItems = Option(schema); this }
    def item(schema: Schema): this.type            = { items += schema; this }
    def minItems(v: Int): this.type                = { minItems = Option(v); this }
    def maxItems(v: Int): this.type                = { maxItems = Option(v); this }
    def additionalItems(schema: Schema): this.type = { additionalItemsSchema = Some(schema); this }
    def additionalItems(allow: Boolean): this.type = { allowAdditionalItems = allow; this }
    def uniqueItems(v: Boolean): this.type         = { uniqueItems = v; this }
    def contains(schema: Schema): this.type        = { contains = Option(schema); this }

    def doBuild(): ArraySchema = new ArrImpl(this)
  }

  class ArrImpl(builder: Arr) extends SchemaImpl(builder) with ArraySchema {
    val array: Boolean                        = builder.arrayType
    val minItems: Option[Int]                 = builder.minItems
    val maxItems: Option[Int]                 = builder.maxItems
    val uniqueItems: Boolean                  = builder.uniqueItems
    val allItems: Option[Schema]              = builder.allItems
    val items: List[Schema]                   = builder.items.result()
    val allowAdditionalItems: Boolean         = builder.allowAdditionalItems
    val additionalItemsSchema: Option[Schema] = builder.additionalItemsSchema
    val contains: Option[Schema]              = builder.contains
  }

  class AnyOf extends Combined[AnyOfSchema] {
    override protected def doBuild(): AnyOfSchema = new CombinedImpl[AnyOfSchema](this) with AnyOfSchema
  }
  class AllOf(synthetic: Boolean) extends Combined[AllOfSchema](synthetic) {
    override protected def doBuild(): AllOfSchema = new CombinedImpl[AllOfSchema](this) with AllOfSchema
  }
  class OneOf extends Combined[OneOfSchema] {
    override protected def doBuild(): OneOfSchema = new CombinedImpl[OneOfSchema](this) with OneOfSchema
  }

}
