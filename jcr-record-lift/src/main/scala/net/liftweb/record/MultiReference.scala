package net.liftweb.record

import _root_.net.liftweb.util._//{Box,Full,Empty,Failure} 
import _root_.net.liftweb.common._
import net.liftweb.util.Helpers.{tryo}

class MultiReference[K, OwnerType <: Record[OwnerType], Other <: KeyedRecord[Other,K]](
  val ref: ReferenceField[K,OwnerType,Other])  extends MultiValued[K,OwnerType] {  
   
  def field = ref
  def foreign = ref.otherMeta
  
  def add(other:Other) {
    addKey(other.primaryKey.value)
  }
  
  def remove(other:Other)  {
    removeKey(other.primaryKey.value)
  }  
  
  def addKey(key:K) {
    val ix = tryo(delegate.indexOf(key)) openOr -1
    if (delegate==null) delegate=Nil
    if (delegate.isEmpty || ix < 0) +:(key) 
  }
  
  def removeKey(key:K) {
    net.liftweb.util.Log.info("Devo rimuovere chiave "+key)
    net.liftweb.util.Log.info("Delegate ora e' "+delegate)
    val ix = tryo(delegate.indexOf(key)) openOr -1
    net.liftweb.util.Log.info("Devo rimuovere key "+ix)
    if (ix >= 0) remove(ix) 
  }   
  
  def moveUp(other:Other) { 
    val ix = delegate.indexOf(other.primaryKey.value)
    if (ix > 0) { 
      val (before,after) = delegate.splitAt(ix - 1)
      delegate = before ++ List(delegate(ix), delegate(ix - 1)) ++ after.drop(2)
    }  
  }
   
  def moveDown(other:Other) { 
    val ix = delegate.indexOf(other.primaryKey.value)
    if (ix < (delegate.length - 1)) { 
      val (before,after) = delegate.splitAt(ix)
      delegate = before ++ List(delegate(ix+1), delegate(ix)) ++ after.drop(2)
    }  
  }
  
  def obj(ix:Int):Box[Other] = {
    if (delegate==null) delegate=Nil
    field(delegate(ix)) 
    field.obj
  }
  
  def objs:List[Other] = {
    if (delegate==null) delegate=Nil
    delegate.flatMap{k => 
    field(k)
    field.obj
  }} 
}
