package net.jfmx.jcr

import _root_.net.liftweb.record.{Field,MultiValued}
import _root_.javax.jcr.{Value,Property,ValueFactory}

trait MultiProperty[T,OwnerType <:JCRNode[OwnerType]] extends MultiValued[T,OwnerType] with JCRProperty[T] {
  def prop: Field[T,OwnerType] with JCRProperty[T]
  def propertyType = prop.propertyType
  override def multiValued  = true
  def fromValue(v:Value) = prop.fromValue(v) 
  def asValue(vf:ValueFactory):Value = prop.asValue(vf)
  override def asValues(vf:ValueFactory):Array[Value] = value.map{v => prop.set(v); asValue(vf)}.toArray

}

class MultiValuedProperty[T,OwnerType <:JCRNode[OwnerType]](single: Field[T,OwnerType] with JCRProperty[T]) 
                         extends MultiProperty[T,OwnerType] {
    def prop = single
    def field = single
}    
