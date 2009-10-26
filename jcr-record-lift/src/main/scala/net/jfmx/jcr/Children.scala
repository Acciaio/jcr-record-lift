package net.jfmx.jcr

import _root_.javax.jcr.{PropertyType,Value,ValueFactory}
import _root_.net.liftweb.record
import scala.xml._

class Children[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](rec:Owner,otherMeta:JCRNodeType[Other]) 
               extends record.OneToMany[String,String,Owner,Other](new ReferenceProperty(rec,otherMeta)) with JCRChildNode[Other] {
  def meta = otherMeta
  override def add(o:Other) {
    o.jcrName = this.owner.jcrName+"/"+this.name+"/"+o.name 
    subNodes(o.name.value.toString) = o 
    super.add(o)
  } 
  
  override def remove(o:Other) {
    subNodes -= o.name.value.toString
    super.remove(o)
  }
  
  override def size = subNodes.size
  
  override def toXHtml: Elem = {
    if (size<=0) <div/>
    else <ul>
    	 {get.map(n => <li>{n.name.value}</li>)}
    	 </ul>
  }
  
  override def asXHtml = toXHtml // per adesso non e' modificabile...almeno non da qui
     
}            
                
class Parent[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](rec:Owner, otherMeta:JCRNodeType[Other])
            extends record.StringReferenceField(rec,otherMeta) with record.ManyToOne[String,String,Owner,Other]

class Child[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](rec:Owner, otherMeta:JCRNodeType[Other])
            extends record.StringReferenceField(rec,otherMeta) with record.OneToOne[String,String,Owner,Other] with JCRChildNode[Other] {
  def meta = otherMeta
  override def multiValued = false 
}
            
class OneToMany[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](ref: ReferenceProperty[Owner,Other]) 
            extends record.OneToMany[String,String,Owner,Other](ref) with MultiProperty[String,Owner] {
  def prop = ref

}
            
class ManyToOne[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](rec:Owner, otherMeta: JCRNodeType[Other]) 
                      extends ReferenceProperty(rec,otherMeta) with record.ManyToOne[String,String,Owner,Other] 
                      
class OneToOne[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](rec:Owner, otherMeta:JCRNodeType[Other])                      
                      extends ReferenceProperty(rec,otherMeta) with record.OneToOne[String,String,Owner,Other]
                      
class ManyToMany[Owner <: JCRNode[Owner], Other <: JCRNode[Other]](ref: ReferenceProperty[Owner,Other])
                extends record.ManyToMany[String,String,Owner,Other](ref) with MultiProperty[String,Owner] {
  def prop = ref                  
}
