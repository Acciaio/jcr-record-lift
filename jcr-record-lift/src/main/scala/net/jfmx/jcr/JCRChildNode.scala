package net.jfmx.jcr

import _root_.scala.collection.mutable
import _root_.javax.jcr.{Session,Node,PathNotFoundException}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import JCRExtensions._

trait JCRChildNode[MyType <: JCRNode[MyType]] extends JCRItem {
  def meta:JCRNodeType[MyType] 
  def multiValued = true   
  val subNodes: mutable.Map[String,MyType] = new mutable.HashMap[String,MyType]
  
  val myJcrPrimaryType = meta.jcrPrimaryType
  
  def get = subNodes.values.toList
  def getKeys = subNodes.keys.toList
  
  def size = subNodes.size
  
  def fromJCR(parent:Node,path: String):Any = Helpers.tryo {
    val node = parent.getNode(jcrName)
    if (multiValued) {
      node.getNodes().foreach(n => {
        val newNode = meta.create(n)
        subNodes(n.getName) = newNode
        newNode.jcrName = path+n.getName}) 
      subNodes.keys
    } else {
    	val newNode = meta.create(node)
        subNodes(jcrName) = newNode
        newNode.jcrName = path+node
        subNodes(jcrName) = meta.create(node)
    }
  } openOr {
    if (multiValued) Nil else null 
  }
  
  def asJCR(session:Session,parent:Node):Unit = {
    try {
      val node = parent.getNode(jcrName)
      if (multiValued) {
        // Delete nodes
        node.getNodes().foreach{n =>  if (!subNodes.contains(n.getName())) n.remove()}
        subNodes.foreach{v => 
          if (node.hasNode(v._2.jcrName)) 
            try {
            	meta.asJCR(session,v._2,node.getNode(v._2.jcrName))
            } catch {
              case _ => 
            }
          else meta.asJCR(session,v._2,node.addNode(v._2.jcrName,myJcrPrimaryType))}
      } else meta.asJCR(session,subNodes(jcrName),parent)  
    } catch {
      case a:PathNotFoundException => { 
        // Creazione
        if (multiValued) {
          val node = if (meta.jcrPrimaryType == "nt:file" || meta.jcrPrimaryType=="nt:folder") parent.addNode(jcrName,"nt:folder")  
                     else parent.addNode(jcrName,meta.jcrPrimaryType)
          subNodes.foreach{ v => meta.asJCR(session,v._2,node.addNode(v._2.jcrName,myJcrPrimaryType))}
        } else meta.asJCR(session,subNodes(jcrName),parent.addNode(jcrName,myJcrPrimaryType))
      }  
    }
 }   
    
}
