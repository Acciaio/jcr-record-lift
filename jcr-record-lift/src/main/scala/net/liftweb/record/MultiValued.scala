package net.liftweb.record

import _root_.scala.collection.{mutable}
//import _root_.net.liftweb.util.{Box,Full,Empty,Failure}
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.http.js.{JE} 
import _root_.scala.xml.{Node}
 
trait MultiValued[T  , OwnerType <: Record[OwnerType]] extends mutable.Buffer[T] with Field[Seq[T],OwnerType] {  
  protected var delegate: List[T] = _
  
  def field: Field[T,OwnerType]
  
  def owner = field.owner 
   
  def defaultValue = Nil
  
  def setFromAny(in:Any): Box[MyType] = {
    in match {
      case seq: Seq[_] => Full(set(seq.map(item => field.setFromAny(item).open_!))) 
      case null => Full(set(Nil))
      case Some(s:String) => field.setFromString(s).flatMap(item => Full(set(List(item))))
      case Full(s:String) => field.setFromString(s).flatMap(item => Full(set(List(item)))) 
      case Node | Empty | Failure(_,_, _) => Full(set(Nil))
      case o => Full(set(Seq(field.setFromAny(o).open_!)))
    }
  }
  
  def setFromString(s:String):Box[MyType] = Full(set(Seq(field.setFromString(s).open_!)))
  
  def asJs = null
  def asXHtml = <span>{value.toString}</span>
  def toForm  = asXHtml
  
  
  def all      = delegate
  def readOnly = all
  def length = delegate.length
  def elements = delegate.elements
  
  def apply(n:Int) = delegate(n)
  
  def +=(item:T) {
    if (delegate==null) delegate = Nil
    delegate += item
    set(delegate.toSeq)
  }
  
  def +:(item:T) = {
    if (delegate==null) delegate = Nil
    delegate ::= item
    set(delegate.toSeq)
    this
  }  
  
  def insertAll(n:Int, iter:Iterable[T]) {
    val (before,after) = delegate.splitAt(n)
    delegate = before ++ iter ++ after
    set(delegate.toSeq)
  }
  
  def update(n:Int, item: T) {
    val (before,after) = (delegate.take(n), delegate.drop(n+1))
    delegate = before ++ List(item) ++ after
    set(delegate.toSeq)
  }
  
  def remove(n:Int) = { 
    val item = delegate(n)
    delegate = delegate.remove( _ == item)
    set(delegate.toSeq)
    item
  }
  
  def clear() {
    while (delegate.length > 0) remove(0)
  }
 
}


