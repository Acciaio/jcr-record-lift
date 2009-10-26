package net.jfmx.jcr

import javax.jcr.{Session,Property,Node,Value,ValueFactory,PathNotFoundException}
import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.js.{JsExp}

trait JCRItem  {
  def stripNS(aName:String) = aName.substring(aName.indexOf(":")+1)
  def addNS(ns:String, aName:String) = ns+":"+aName 
  
  // Verificare con beppe come voleva che diventasse il jcrName
  // Vediamo se lo trasformo in una var per gestire i figli
  var /*def*/ jcrName:String = this.getClass.getName.split("\\$").last//.substring(0,this.getClass.getName.lastIndexOf('$'))
  
  def autoCreated = false
  def mandatory = false
  //def onParentVersion = VM.COPY
  def isProtected = false
  
}
     
trait JCRProperty[MyType] extends JCRItem { 
  def propertyType:Int
  
  def multiValued = false
  
  // Concrete class should define this for type conversion
  def fromValue(v:Value):MyType
  def asValue(vf:ValueFactory):Value 
 
  // Multiple values management
  def fromValues(vs:Array[Value]):Seq[MyType]  = vs.map{v => fromValue(v)}
  def asValues(vf:ValueFactory):Array[Value] = throw new JCRException("Method defined only on multi valued property")
 
  //Concrete class should define these two function for access to field.value
  def fromJCR(node:Node):Any = Helpers.tryo {
    val p = node.getProperty(jcrName)    
    if (multiValued) fromValues(p.getValues())
    else fromValue(p.getValue())             
  } openOr  {
    if (multiValued) Nil else null
  } 
   
  def asJCR(session:Session, node:Node):Property = {
    if (multiValued) node.setProperty(jcrName,asValues(session.getValueFactory))
    else node.setProperty(jcrName,asValue(session.getValueFactory))
  }
  
}
