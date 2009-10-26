package net.jfmx.jcr

import _root_.java.util.{Calendar}
import _root_.net.liftweb.record._
import _root_.net.liftweb.record.field._ 
import _root_.javax.jcr.{Node,Property,Value,PropertyType,ValueFactory}
import _root_.net.liftweb.http.{S}
import S._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._
 
class StringProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType, maxLength:Int) 
	extends StringField(rec,maxLength) 
 	with JCRProperty[String] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.STRING
  def fromValue(v:Value) = v.getString()
  def asValue(vf:ValueFactory) = vf.createValue(value)

}

  // Posso prendere in ingresso una stringa come dateformat...
class DateTimeProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType, df: String) 
	extends DateTimeField(rec) 
 	with JCRProperty[Calendar] 
 	with DBSearchable[OwnerType] {
 	  
  def this(rec:OwnerType) = this(rec,"HH:mm:ss dd/MM/yyyy")
 	  
  def propertyType = PropertyType.DATE
  def fromValue(v:Value) = v.getDate()
  def asValue(vf:ValueFactory) = //vf.createValue(value)
  	new org.apache.jackrabbit.value.DateValue(value)
  
  override def setFromAny(f : Any) = (f) match {
    case c : Full[_] => (c.open_!) match {
      case cal : Calendar => this.set(cal); Full(cal)
      case _ => setFromAny(c.open_!)
    }
    case s: String => try {
    					setFromAny((new java.text.SimpleDateFormat(df)).parse(s))
    				  } catch {
    				    case _  => super.setFromAny(s)
    				  }
    case _ => {
        net.liftweb.util.TimeHelpers.toDate(f).map(d => {
    	val cal = java.util.Calendar.getInstance()
    	cal.setTime(d)
    	this.set(cal)
    	})
        }
    }

  private def elem =
  S.fmapFunc(SFuncHolder(this.setFromAny(_))){funcName =>
    <input type="text"
      name={funcName}
      value={value match {case null => "" case s: Calendar => (new java.text.SimpleDateFormat(df).format(s.getTime))}}
      tabindex={tabIndex toString}/>
  }
  
  override def toForm = {
    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }
  }

  override def asXHtml = {
    var el = elem
    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }
  
  override def fromJCR(node:Node):Any = net.liftweb.util.Helpers.tryo {
    val p = node.getProperty(jcrName)    
    fromValue(p.getValue())             
  } 
  override def asJCR(session:javax.jcr.Session, node:Node):Property = {
    node.setProperty(jcrName,/*(new org.apache.jackrabbit.value.DateValue(value))*/asValue(session.getValueFactory)) 
  }
  
  override def toXHtml = scala.xml.Text((new java.text.SimpleDateFormat(df)).format(value.getTime))
}
 
class LongProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType) 
	extends LongField(rec) 
 	with JCRProperty[Long] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.LONG
  override def fromValue(v:Value) = v.getLong()
  override def asValue(vf:ValueFactory) = vf.createValue(value.toLong)

}

class DoubleProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType) 
	extends DoubleField(rec) 
 	with JCRProperty[Double] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.DOUBLE
  override def fromValue(v:Value) = v.getDouble()
  override def asValue(vf:ValueFactory) = vf.createValue(value.toDouble)
  
}

class IntProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType) 
	extends IntField(rec) 
 	with JCRProperty[Long] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.LONG
  override def fromValue(v:Value) = v.getLong().toInt
  override def asValue(vf:ValueFactory) = vf.createValue(value.toLong)

}

class EnumProperty[OwnerType <: JCRNode[OwnerType], ENUM <: Enumeration](rec:OwnerType, enum:ENUM) 
	extends EnumField(rec,enum) 
 	with JCRProperty[Long] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.LONG
  override def fromValue(v:Value) = enum(v.getLong().toInt).id.toLong
  override def asValue(vf:ValueFactory) = vf.createValue(value.id.toLong)

}

class BooleanProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType) 
	extends BooleanField(rec) 
 	with JCRProperty[Boolean] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.BOOLEAN
  override def fromValue(v:Value) = v.getBoolean()
  override def asValue(vf:ValueFactory) = vf.createValue(value)

}

class ReferenceProperty[OwnerType <: JCRNode[OwnerType], Other <: JCRNode[Other]](rec:OwnerType, otherMeta: JCRNodeType[Other]) 
	extends StringReferenceField(rec,otherMeta) 
    with JCRProperty[String] 
    with DBSearchable[OwnerType] {
  def propertyType = PropertyType.PATH
  override def fromValue(v:Value) = v.getString()
  override def asValue(vf:ValueFactory) = vf.createValue(value)

}
                      
trait DBSearchable[OwnerType <: JCRNode[OwnerType]] extends OwnedField[OwnerType] {
  self: JCRItem =>
  
  lazy val dbSelectString = /*owner.meta.internalClassName +*/ "@" + jcrName.split("\\$").last
  
}