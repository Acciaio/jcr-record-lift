package net.liftweb.record

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import _root_.java.util.regex._
import S._
import Helpers._
import JE._

abstract class ViewRecordField [MyType,OwnerType <: Record[OwnerType]](rec: OwnerType, field: Field[MyType,OwnerType]) extends Field[MyType, OwnerType] {

  override def canWrite_? = false
  //override def ignoreField_? = true
  //override def canRead_? = false
  
  def owner = rec

  def setFromString(s: String): Box[MyType] = field.setFromString(s)
  
  def setFromAny(in: Any): Box[MyType] = field.setFromAny(in)
  
  private def elem = S.fmapFunc(SFuncHolder(field.setFromAny(_))) {
    funcName =>
    <input type="text" name={funcName}
      value={field.value match {case null => "" case s => s.toString}}
      tabindex={tabIndex toString}/>
  }

  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }
  
  override def toXHtml = field.toXHtml

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }
  
  override def apply(in: MyType): OwnerType = if (owner.meta.mutable_?) {
		  field.set(in)
		  owner
	  } else {
		  owner.meta.createWithMutableField(owner, this, in)
	  }


  def defaultValue: MyType = field.value
  
  def asJs = Str(value.toString)

}