package org.mulesoft.scheletor.jsonschema

sealed trait SpecVersion {
  def id: String
  def idKeyword: String         = "$id"
  def supportPropertyNames      = true
  def supportArrayContains      = true
  override def toString: String = id
}

object Draft4 extends SpecVersion {
  val id                                     = "Draft4"
  override def supportPropertyNames          = false
  override def supportArrayContains: Boolean = false
  override def idKeyword: String             = "id"
}

object Draft6 extends SpecVersion {
  val id = "Draft6"
}

object Draft7 extends SpecVersion {
  val id = "Draft7"
}
