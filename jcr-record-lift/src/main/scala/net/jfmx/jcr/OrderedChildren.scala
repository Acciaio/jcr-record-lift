package net.jfmx.jcr

import _root_.javax.jcr._
import _root_.net.liftweb.record._
import _root_.scala.collection.mutable
import _root_.javax.jcr.{Session,Node,PathNotFoundException}
import _root_.net.liftweb.util._//{Helpers,Box,Full,Empty,Failure}
import _root_.net.liftweb.common._
import JCRExtensions._
import _root_.net.liftweb.record._
import _root_.net.liftweb.record.field._
//import scala.xml._
import _root_.net.liftweb.util._
import Helpers._

class OrderedChildren[Owner <: JCRNode[Owner], Other <: JCROrderedNode[Other] with JCRNode[Other]](rec:Owner,otherMeta:JCRNodeType[Other])
	extends net.liftweb.record.OneToMany[String,String,Owner,Other](new ReferenceProperty(rec,otherMeta))
	with JCRChildNode[Other] {
  
  def meta = otherMeta
  
  override def size = subNodes.size
  
  override def get = getOrdered
  override def getKeys = getOrdered.map(n => n.name.value.toString)
  
  override def toXHtml: scala.xml.Elem = {
    if (size<=0) <div/>
    else <ul>
    	 {get.map(n => <li>{n.name.value}</li>)}
    	 </ul>
  }
  
  override def asXHtml = toXHtml // per adesso non e' modificabile...almeno non da qui
  
  //aggiunge in coda
  override def add(o:Other) {
    o.jcrName = this.owner.jcrName+"/"+this.name+"/"+o.name
    if (!subNodes.values.toList.isEmpty)
      (subNodes.values.toList.find(n => {
        (n.next.value==null || n.next.value=="")
      })) match {
        case Some(last) => {
          last.next.setFromAny(o.name.value.toString)
          last.save
        }
        case _ => }
    subNodes(o.name.value.toString) = o
    super.add(o)
  }
  
  def addAt(o:Other, pos: Int): Boolean = {
    o.jcrName = this.owner.jcrName+"/"+this.name+"/"+o.name
    val values = get
    if (values.isEmpty)
      add(o)
    else {
      if (pos<=0)
        o.next.setFromAny(values.first.name.value.toString)
      else 
      try {
        val prec = tryo{values(pos-1)} openOr values.last
        o.next.setFromAny(prec.next.value.toString)
        prec.next.setFromAny(o.name.value.toString)
        prec.save
      } catch {
        case _ => false
      }
    }
    subNodes(o.name.value.toString) = o
    super.add(o)
    true
  }
  
  // Ancora da debuggare
  def remove(key: String): Boolean = {
    (get.find(n => n.name.value.toString == key)) match {
      case Some(elem) => {
        remove(elem)
        true
      }
      case _ => false
    }
  }
  override def remove(o:Other)= {
    if (subNodes.size>1) 
      (get.find(n => n.next.value.toString==o.name.value.toString)) match {
        case Some(prec) => prec.next.setFromAny(o.next.value)
        case _ => }
    subNodes -= o.name.value.toString
    super.remove(o)
    o.delete_!
  } 
  
  def getOrdered: List[Other] = {
	(subNodes.values.toList.find(n => (n.next.value=="" || n.next.value==null))) match {
        case Some(last) => {
        	val tempNodes = subNodes.clone
        	makeList(tempNodes.values.toList,last.name.value.toString,List(last))
         }
        case _ => {List()}
     }
  }
 
  def makeList(in: List[Other], lastEx: String, out: List[Other]):List[Other] = {
      (in.find(n => n.next.value.toString==lastEx)) match {
        case Some(n) => {
          makeList(in, n.name.value.toString, out ::: List(n) )
        }
        case _ => {
          if (out.isEmpty) in
          else out.reverse
        }
      }
  }

}

trait JCROrderedNode[OwnerType <: JCRNode[OwnerType]] {
  self: OwnerType =>
  
  /* Solo per compilare correttamente con eclipse */
  lazy val _self: OwnerType = this
  
  object next extends StringProperty(_self,32) {}
  
}
 
