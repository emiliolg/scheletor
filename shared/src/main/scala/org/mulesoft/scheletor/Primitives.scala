package org.mulesoft.scheletor

/**
  * Implement ObjLike type classes for some primitives
  */
object Primitives {
  abstract class Scalar[T] extends ObjLike[T] {
    override def isNull(v: T): Boolean       = v == null
    override def asScalar(v: T): Option[Any] = Option(v)
  }

  implicit object ObjLikeString extends Scalar[String] {
    override def asString(v: String): Option[String] = Option(v)
  }

  implicit object ObjLikeBoolean extends Scalar[Boolean] {
    override def asBoolean(v: Boolean): Option[Boolean] = Option(v)
  }

  implicit object ObjLikeInt extends Scalar[Int] {
    override def asInt(v: Int): Option[Int] = Option(v)
  }

  implicit object ObjLikeLong extends Scalar[Long] {
    override def asLong(v: Long): Option[Long] = Option(v)
  }

  implicit object ObjLikeDouble extends Scalar[Double] {
    override def asDouble(v: Double): Option[Double] = Option(v)
  }
}
