package org.mulesoft.scheletor

import org.yaml.model.{YMap, YNode, YSequence}

trait ObjLike[V] {
  def isNull(v: V): Boolean
  def asBoolean(v: V): Option[Boolean]        = None
  def asString(v: V): Option[String]          = None
  def asLong(v: V): Option[Long]              = None
  def asInt(v: V): Option[Int]                = None
  def asDouble(v: V): Option[Double]          = None
  def asScalar(v: V): Option[Any]             = None
  def asObject(v: V): Option[ObjLike.Obj[V]]  = None
  def isObject(v: V): Boolean                 = asObject(v).isDefined
  def asArray(v: V): Option[ObjLike.Array[V]] = None
}

object ObjLike {

  implicit object ObjLikeString extends ObjLike[String] {
    override def isNull(v: String): Boolean          = v == null
    override def asString(v: String): Option[String] = Option(v)
    override def asScalar(v: String): Option[Any]    = Option(v)
  }

  implicit object ObjLikeYNode extends ObjLike[YNode] {
    override def isNull(node: YNode): Boolean         = node == null || node.isNull
    override def asBoolean(v: YNode): Option[Boolean] = v.asOption[Boolean]
    override def asString(v: YNode): Option[String]   = v.asOption[String]
    override def asInt(v: YNode): Option[Int]         = v.asOption[Int]
    override def asLong(v: YNode): Option[Long]       = v.asOption[Long]
    override def asDouble(v: YNode): Option[Double]   = v.asOption[Double]
    override def asScalar(v: YNode): Option[Any]      = v.asOption[Any]
    override def isObject(v: YNode): Boolean          = v.value.isInstanceOf[YMap]
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

  implicit class ObjOps[V](val v: V) extends AnyVal {
    def isNull(implicit o: ObjLike[V]): Boolean            = v == null || o.isNull(v)
    def asObject(implicit o: ObjLike[V]): Option[Obj[V]]   = o.asObject(v)
    def isObject(implicit o: ObjLike[V]): Boolean          = o.isObject(v)
    def asArray(implicit o: ObjLike[V]): Option[Array[V]]  = o.asArray(v)
    def asBoolean(implicit o: ObjLike[V]): Option[Boolean] = o.asBoolean(v)
    def asString(implicit o: ObjLike[V]): Option[String]   = o.asString(v)
    def asLong(implicit o: ObjLike[V]): Option[Long]       = o.asLong(v)
    def asInt(implicit o: ObjLike[V]): Option[Int]         = o.asInt(v)
    def asDouble(implicit o: ObjLike[V]): Option[Double]   = o.asDouble(v)
    def asScalar(implicit o: ObjLike[V]): Option[Any]      = o.asScalar(v)
  }

  private final val emptyObjSingleton = new Obj[Any] {
    override def properties: Seq[String]     = Seq.empty
    override def get(key: String): Option[_] = None
  }

  def emptyObj[V]: Obj[V] = emptyObjSingleton.asInstanceOf[Obj[V]]

  def deepEquals[V: ObjLike, W: ObjLike](a: V, b: W): Boolean =
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) true
    else if (a.isNull) b.isNull
    else if (b.isNull) false
    else
      a.asScalar match {
        case Some(sa) => b.asScalar contains sa
        case _ =>
          a.asArray match {
            case Some(aa) =>
              b.asArray exists (ab => (aa zip ab) forall (t => deepEquals(t._1, t._2)))
            case _ =>
              a.asObject exists { oa =>
                b.asObject exists { ob =>
                  val oap = oa.properties
                  val obp = ob.properties
                  oap.length == obp.length && (oap forall obp.contains) && oap.forall(p => deepEquals(oa(p), ob(p)))
                }
              }
          }
      }

  trait Obj[V] {
    def properties: Seq[String]
    def get(key: String): Option[V]
    def apply(key: String): V = get(key).get
  }

  trait Array[V] extends IndexedSeq[V] {
    def length: Int
    def apply(offset: Int): V
  }

}
