package net.liftweb.record

import scala.xml._
import net.liftweb.util._
import _root_.net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import _root_.java.util.regex._
import S._
import Helpers._
import JE._

/**
 * A TRANSIENT Field only for view purposes 
 */
abstract class ViewOnlyField[MyType, OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[MyType , OwnerType] {

  def owner = rec

  def setFromString(s: String): Box[MyType] = Empty
  def setFromAny(a: Any): Box[MyType] = Empty                                   
  
  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
    funcName =>
    <label name={funcName} tabindex={tabIndex toString}>{value match {case null => "" case s => s.toString}}</label>
  }

  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }

  def defaultValue: MyType

  def asJs = Str(value.toString)

}