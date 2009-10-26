package net.liftweb.record

import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.util._//{Box,Empty,Full}
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.S._
 
trait ReferenceField[KeyType, OwnerType <: Record[OwnerType], Other <: KeyedRecord[Other,KeyType]] extends Field[KeyType,OwnerType] { 
  def otherMeta:KeyedMetaRecord[KeyType,Other]
  
  def apply(item:Other):OwnerType = {
    apply(item.primaryKey.value)
  }
  
  override def equals(other:Any) = other match { 
    case kr: KeyedRecord[_,_] => this.value == kr.primaryKey.value
    case _ => super.equals(other)
  }
  
  def validSelectedValues: Box[List[(KeyType,String)]] = Empty 
  
  def immutableMsg: NodeSeq = Text(?("Can't change"))
  
  def defined_? :Boolean = data != defaultValue
  
  def cached_? : Boolean = synchronized(_calcedObj)
  
  def obj: Box[Other] = synchronized {
    if (!_calcedObj) {
      _calcedObj = true
      this._obj = if (defined_?) otherMeta.find(data) else Empty
    }
    _obj
  }
  
  def primeObj(obj: Box[Other]) = synchronized {
    _obj = obj
    _calcedObj = true
  }
  
  private var _obj: Box[Other] = Empty
  private var _calcedObj = false

  def setKey(in:MyType) { 
    if (in != value) {
      _calcedObj = false  
      _obj = Empty
      set(in)
    }  
  }  
  
  def resetKey:Unit = setKey(defaultValue)
  
  def reset:Unit = resetKey
  def set(other:Other) = setKey(other.primaryKey.value)
  
}

import field.{StringField,LongField}


class StringReferenceField[Owner <: Record[Owner], Other <: KeyedRecord[Other,String]](rec:Owner, meta:KeyedMetaRecord[String,Other]) 
                          extends StringField(rec,32) with ReferenceField[String,Owner,Other] {
  def otherMeta = meta
}

class LongReferenceField[Owner <: Record[Owner], Other <: KeyedRecord[Other,Long]](rec:Owner, meta:KeyedMetaRecord[Long,Other]) 
                          extends LongField(rec) with ReferenceField[Long,Owner,Other] {
  def otherMeta = meta
}
