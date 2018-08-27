package org.mule.scheletor

import org.mule.scheletor.Pointer.{Idx, Prop}
import ObjLike._

import scala.language.implicitConversions

case class Pointer private (path: List[Pointer.Node]) {

  def /(node: Pointer.Node): Pointer   = Pointer(path :+ node)
  def /(propertyName: String): Pointer = this / Prop(propertyName)
  def /(idx: Int): Pointer             = this / Idx(idx)

  def ++(pointer: Pointer): Pointer = new Pointer(path ++ pointer.path)

  def extract[V: ObjLike](v: V): Option[V] = Pointer.extract(path, v)
  def toList: List[Pointer.Node]           = path

  override def toString: String = path.mkString("/", "/", "")
}

object Pointer extends Pointer(Nil) {

  private final val Numeric = "[1-9][0-9]*".r

  def empty: Pointer = this

  def apply(path: String): Pointer =
    if ((path eq null) || path.isEmpty || path == "/") empty
    else
      Pointer((path split "/").toList.tail map {
        case "0"           => Idx(0)
        case s @ Numeric() => Idx(s.toInt)
        case s             => Prop(s.replace("~1", "/").replace("~0", "~"))
      })

  def apply(path: Node*): Pointer = if (path.isEmpty) empty else new Pointer(path.toList)

  sealed trait Node

  case class Idx(idx: Int) extends Node {
    override def toString: String = idx.toString
  }

  implicit def intToIdx(n: Int): Pointer.Idx          = Pointer.Idx(n)
  implicit def strToPropNode(s: String): Pointer.Prop = Pointer.Prop(s)

  case class Prop(name: String) extends Node {
    override def toString: String = name.replace("~", "~0").replace("/", "~1")
  }
  private def extract[V: ObjLike](path: List[Pointer.Node], v: V): Option[V] = path match {
    case Nil                           => Some(v)
    case (x: Prop) :: xs if v.isObject => v.asObject flatMap (_.get(x.name)) flatMap (extract(xs, _))
    case (x: Idx) :: xs                => v.asArray flatMap (_.get(x.idx)) flatMap (extract(xs, _))
    case _                             => None

  }
}
