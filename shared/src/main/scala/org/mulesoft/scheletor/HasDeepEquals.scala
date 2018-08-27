package org.mulesoft.scheletor

import ObjLike._

trait HasDeepEquals[V] {

  def deepEquals[W: ObjLike](a: V, b: W): Boolean = {
    implicit val o: ObjLike[V] = toObjLike(a)
    a === b
  }

  protected def toObjLike(v: V): ObjLike[V]
}
