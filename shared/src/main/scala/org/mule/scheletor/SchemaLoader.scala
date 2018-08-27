package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.ObjLike._
import org.mule.scheletor.SchemaBuilder._
import org.mule.scheletor.SchemaLoader.{Builder, Exception, Recognizer}

import scala.collection.mutable

object SchemaLoader {
  type Builder = SchemaBuilder[_ <: Schema]

  trait Recognizer {
    def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder]
  }
  case class Exception(pointer: Pointer, errorType: ErrorType) extends RuntimeException(errorType.toString)

}

abstract class SchemaLoader[V: ObjLike](private val input: V, pointer: Pointer = Pointer.empty) { outer =>

  protected val obj: Obj[V] = input.asObject.getOrElse(emptyObj)
  private val properties    = obj.properties.to[mutable.Set]

  def error(loc: Pointer, errorType: ErrorType): Nothing = throw Exception(pointer ++ loc, errorType)
  def error(loc: String, errorType: ErrorType): Nothing  = throw Exception(pointer / loc, errorType)

  final def loadSubSchema(input: V, loc: String): Schema  = createChildLoader(input, pointer / loc).load()
  final def loadSubSchema(input: V, loc: Pointer): Schema = createChildLoader(input, pointer ++ loc).load()

  def typeError(loc: Pointer, referenceName: String, typeName: String): Nothing =
    throw Exception(pointer ++ loc, BadType(referenceName, typeName))

  def load(): Schema = {
    val builder = createBuilder()
    addCommonProperties(builder)
    builder.build
  }

  def get(key: String): Option[Value] =
    if (properties contains key) Some(value(key)) else None

  def extract(pointer: Pointer): Option[V] = input.extract(pointer)

  def ifPresent(key: String)(op: Value => Unit): Unit =
    if (properties contains key) op(value(consume(key)))

  def canBeSchema(v: V): Boolean = v.isObject

  def containsAnyOf(ps: Set[String]): Boolean = properties exists ps.contains

  def schemaFor(keyword: String): Option[Schema] = get(keyword).map(_.loadSubSchema())

  def rootLoader: SchemaLoader[V]

  protected def createChildLoader(input: V, loc: Pointer): SchemaLoader[V]

  final protected def consume(key: String): String = { properties -= key; key }

  protected def addCommonProperties(builder: Builder): Unit

  protected def recognizers: List[Recognizer]

  private def createBuilder(): Builder = recognizers.flatMap(_.extract(this)) match {
    case Nil     => emptySchema
    case List(s) => s
    case l       => allOfSchema(true).schemas(l)
  }

  private def value(key: String): Value = Value(obj(key), key)

  case class Value(value: V, name: String) {
    def asBoolean: Boolean = convertTo[Boolean](_.asBoolean, "Boolean")
    def asString: String   = convertTo[String](_.asString, "String")
    def asInt: Int         = convertTo[Int](_.asInt, "Int")
    def asDouble: Double   = convertTo[Double](_.asDouble, "Double")
    def asArray: Array[V]  = convertTo[Array[V]](_.asArray, "Array")
    def asObject: Obj[V]   = convertTo[Obj[V]](_.asObject, "Object")
    def isObject: Boolean  = value.isObject
    def isArray: Boolean   = value.isArray

    def asStringSeq: Seq[String] = {
      val a = asArray
      for (idx <- 0 until a.length)
        yield a(idx).asString.getOrElse(error(idx, "String"))
    }
    def asSchemaSeq: Seq[Schema] = {
      val a = asArray
      for (idx <- 0 until a.length) yield {
        val o = a(idx)
        if (canBeSchema(o)) outer.loadSubSchema(o, Pointer(name, idx))
        else error(idx, "Not an Schema")
      }
    }

    def error[T](typeName: String): Nothing           = typeError(Pointer / name, name, typeName)
    def error[T](idx: Int, typeName: String): Nothing = typeError(Pointer(name, idx), name + "[" + idx + "]", typeName)

    def loadSubSchema(): Schema = if (canBeSchema(value)) outer.loadSubSchema(value, name) else error("Not an Schema")

    private def convertTo[T](convert: V => Option[T], typeName: String): T =
      convert(value).getOrElse(error(typeName))

  }
}
