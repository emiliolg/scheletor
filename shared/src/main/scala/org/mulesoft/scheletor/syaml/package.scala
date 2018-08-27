package org.mulesoft.scheletor
import org.yaml.model.{YMap, YNode, YSequence}
import ObjLike._

package object syaml {

  /**
    * Implementation of type class ObjLike for using syaml YNode type as the input
    */
  implicit object ObjLikeYNode extends ObjLike[YNode] {
    override def isNull(node: YNode): Boolean         = node == null || node.isNull
    override def isArray(v: YNode): Boolean           = v.value.isInstanceOf[YSequence]
    override def isObject(v: YNode): Boolean          = v.value.isInstanceOf[YMap]
    override def asBoolean(v: YNode): Option[Boolean] = v.asOption[Boolean]
    override def asString(v: YNode): Option[String]   = v.asOption[String]
    override def asInt(v: YNode): Option[Int]         = v.asOption[Int]
    override def asLong(v: YNode): Option[Long]       = v.asOption[Long]
    override def asDouble(v: YNode): Option[Double]   = v.asOption[Double]
    override def asScalar(v: YNode): Option[Any]      = v.asOption[Any]
    override def asObject(v: YNode): Option[Obj[YNode]] = v.value match {
      case m: YMap => Some(new YNodeObj(m))
      case _       => None
    }
    override def asArray(v: YNode): Option[Array[YNode]] = v.value match {
      case s: YSequence => Some(new YNodeArray(s))
      case _            => None
    }

    class YNodeObj(m: YMap) extends Obj[YNode] {
      def properties: Seq[String]            = m.entries.flatMap(e => e.key.asOption[String])
      def get(key: String): Option[YNode]    = m.map.get(key)
      override def apply(key: String): YNode = m.map.apply(key)
    }
    class YNodeArray(sequence: YSequence) extends Array[YNode] {
      override def length: Int               = sequence.nodes.length
      override def apply(offset: Int): YNode = sequence.nodes(offset)
    }
  }
}
