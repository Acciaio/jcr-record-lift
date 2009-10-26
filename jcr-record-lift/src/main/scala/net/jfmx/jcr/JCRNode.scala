package net.jfmx.jcr
 
import _root_.javax.jcr.{Node}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.record.{KeyedRecord,KeyField}
import _root_.net.liftweb.record.field.{StringField}
import _root_.net.liftweb.mapper.{Safe}

/** 
 * Define the behaviour of repository node instances 
 */

trait JCRNode[T <: JCRNode[T]] extends KeyedRecord[T,String] with JCRItem {
  self:T =>      
  
  private var jcrSessionIdentifier: Box[SessionIdentifier] = Empty
 
  private var was_deleted_? = false
 
  def meta:JCRNodeType[T]
  
  implicit def thisToModelObject(in: JCRNode[T]): T = this.asInstanceOf[T] 
   
  def sessionIdentifier(id:SessionIdentifier):T = {
    if (id != meta.jcrDefaultSessionIdentifier || jcrSessionIdentifier.isDefined) jcrSessionIdentifier = Full(id)
    thisToModelObject(this) 
  }
  
  def sessionIdentifier = jcrSessionIdentifier openOr meta.jcrDefaultSessionIdentifier
  
  def saved_? : Boolean = meta.saved_?(this)
   
  def jcrCanDelete_? : Boolean = meta.saved_?(this) && !was_deleted_?
    
  def dirty_? : Boolean = meta.dirty_?(this) 
  
  def save():Boolean = {
    runSafe {
      meta.save(this)
    }
  }
  
  def delete_!():Boolean = {
    runSafe {
      meta.delete_!(this)
    }
  }
  
  def fromJCR(node:Node) = meta.fromJCR(this,node)
  
  /*override def*/ jcrName/*:String*/ = name.value.toString
  
  object name extends StringField(this,32) with KeyField[String,T] {}

  lazy val primaryKey = name 
  
}
 