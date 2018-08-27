package org.mule.scheletor

/**
  * An ObjLike is a type class for defining Object Like structures with the follow basic structure:
  *
  * {{{
  *
  * ObjLike ::= Scalar
  *          | Array ObjLike
  *          | Obj String ObjLike                 (A Composite Object something like a Map[String,ObjLike])
  * Scalar ::= Null | String | Boolean | Number
  * Number ::= Int | Long | Double
  *
  * }}}
  */
trait ObjLike[V] {
  def isNull(v: V): Boolean
  def isArray(v: V): Boolean                  = asArray(v).isDefined
  def isObject(v: V): Boolean                 = asObject(v).isDefined
  def asBoolean(v: V): Option[Boolean]        = None
  def asString(v: V): Option[String]          = None
  def asLong(v: V): Option[Long]              = asInt(v).map(_.asInstanceOf[Long])
  def asInt(v: V): Option[Int]                = None
  def asDouble(v: V): Option[Double]          = None
  def asScalar(v: V): Option[Any]             = None
  def asObject(v: V): Option[ObjLike.Obj[V]]  = None
  def asArray(v: V): Option[ObjLike.Array[V]] = None
}

object ObjLike {

  // Inner traits

  /** Composite Object similar to a {{{Map[String,ObjLike]}}} */
  trait Obj[V] {
    def properties: Seq[String]
    def get(key: String): Option[V]
    def apply(key: String): V = get(key).get
  }

  /** An Array of Objects like {{{Array[Object]}}} */
  trait Array[V] extends IndexedSeq[V] {
    def length: Int
    def apply(offset: Int): V
    def get(offset: Int): Option[V] = if (offset>=0 && offset<length) Some(apply(offset)) else None
  }

  /** The empty composite Object */
  def emptyObj[V]: Obj[V] = emptyObjSingleton.asInstanceOf[Obj[V]]

  private final val emptyObjSingleton = new Obj[Any] {
    override def properties: Seq[String]     = Seq.empty
    override def get(key: String): Option[_] = None
  }

  /** ObjOps implements operations on V:ObjLike mostly by delegation to the ObjLike[V] trait */
  implicit class ObjOps[V](val v: V) extends AnyVal {
    def isNull(implicit o: ObjLike[V]): Boolean            = v == null || o.isNull(v)
    def isArray(implicit o: ObjLike[V]): Boolean           = o.isArray(v)
    def isObject(implicit o: ObjLike[V]): Boolean          = o.isObject(v)
    def asObject(implicit o: ObjLike[V]): Option[Obj[V]]   = o.asObject(v)
    def asArray(implicit o: ObjLike[V]): Option[Array[V]]  = o.asArray(v)
    def asBoolean(implicit o: ObjLike[V]): Option[Boolean] = o.asBoolean(v)
    def asString(implicit o: ObjLike[V]): Option[String]   = o.asString(v)
    def asLong(implicit o: ObjLike[V]): Option[Long]       = o.asLong(v)
    def asInt(implicit o: ObjLike[V]): Option[Int]         = o.asInt(v)
    def asDouble(implicit o: ObjLike[V]): Option[Double]   = o.asDouble(v)
    def asScalar(implicit o: ObjLike[V]): Option[Any]      = o.asScalar(v)

    /**
      * A deep equals comparation between to ObjLike values
      */
    def ===[W: ObjLike](w: W)(implicit o: ObjLike[V]): Boolean =
      if (v.asInstanceOf[AnyRef] eq w.asInstanceOf[AnyRef]) true
      else if (v.isNull) w.isNull
      else if (w.isNull) false
      else
        v.asScalar match {
          case Some(sa) => w.asScalar contains sa
          case _ =>
            v.asArray match {
              case Some(aa) =>
                w.asArray exists (ab => (aa zip ab) forall (t => t._1 === t._2))
              case _ =>
                v.asObject exists { oa =>
                  w.asObject exists { ob =>
                    val oap = oa.properties
                    val obp = ob.properties
                    oap.length == obp.length && (oap forall obp.contains) && oap.forall(p => oa(p) === ob(p))
                  }
                }
            }
        }
    /** Deep not equals */
    def !==[W: ObjLike](w: W)(implicit o: ObjLike[V]): Boolean = !(v === w)

    /** Given a [[org.mule.scheletor.Pointer]] navigates to the element defined by it */
    def extract(pointer: Pointer)(implicit o: ObjLike[V]): Option[V] = pointer.extract(v)
    def extract(pointer: String)(implicit o: ObjLike[V]): Option[V] = extract(Pointer(pointer))
  }
}
