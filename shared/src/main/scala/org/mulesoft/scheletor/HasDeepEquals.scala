package org.mulesoft.scheletor

trait HasDeepEquals[V] {

  def deepEquals[W: ObjLike](a: V, b: W): Boolean = ObjLike.deepEquals(a, b)(toObjLike(a), implicitly[ObjLike[W]])

  protected def toObjLike(v: V): ObjLike[V]
}
