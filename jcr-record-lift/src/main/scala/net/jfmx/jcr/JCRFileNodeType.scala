package net.jfmx.jcr

import _root_.scala.collection.mutable
import _root_.javax.jcr.{Session,Node,PathNotFoundException}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import JCRExtensions._
import _root_.net.liftweb.record._
import _root_.net.liftweb.record.field._ 
import java.io.{File,InputStream,ByteArrayInputStream}
import javax.jcr._
import _root_.net.liftweb.http.js._
import org.apache.jackrabbit.core.value.{BinaryValueImpl}

trait JCRFileNodeType[NT <: JCRFileNode[NT]] extends JCRNodeType[NT] {
  self: NT =>
  
  override def jcrPrimaryType = "nt:file"
  
  val mimeTypeType = "jcr:mimeType"
  val encodingType = "jcr:encoding"
  val fileType = "jcr:data"
  
  override def asJCR(session:Session, inst:NT, node:Node):Node = {
    
	val resourceNode = node.addNode("jcr:content","nt:resource")

	resourceNode.setProperty(mimeTypeType, inst.mimeType.value)
    resourceNode.setProperty(encodingType, inst.encoding.value)
    resourceNode.setProperty(fileType, inst.file.getInputStream)
    val lastModified = java.util.Calendar.getInstance()
    lastModified.setTimeInMillis((new java.util.Date).getTime)
    resourceNode.setProperty("jcr:lastModified", lastModified)
    
    for (f <- fields(inst) if (f.name!="file" && f.name!="mimeType" && f.name!="encoding")) {
      f match { 
        case prop:JCRProperty[_] => prop.asJCR(session,node) 
        case child:JCRChildNode[_] => child.asJCR(session,node)  
        case e => {}
      }
    }
    node
  }
  
  override def fromJCR(inst:NT, node:Node):Box[NT] = Helpers.tryo{
    val content = node.getNode("jcr:content")
    val mimeI = content.getProperty(mimeTypeType)
    inst.mimeType.setFromAny(mimeI.getValue())
    val encodingI = content.getProperty(encodingType)
    inst.encoding.setFromAny(encodingI.getValue())
    val fileI = content.getProperty(fileType)
    inst.file.setFromAny(fileI.getValue())
    
    inst.name(node.getName)
    for (f <- fields(inst) if (f.name!="file" && f.name!="mimeType" && f.name!="encoding")) {
      f match {
        case prop:JCRProperty[_] => f.setFromAny(prop.fromJCR(node))
        case child:JCRChildNode[_] => {
          //f.setFromAny(child.fromJCR(node))
          val path = inst.jcrName+"/"+f.name+"/"
          f.setFromAny(child.fromJCR(node,path))
        }
        case e => {}
      }
    }
    inst 
  }

}

trait JCRFolderNodeType[NT <: JCRFolderNode[NT,F], F <: JCRFileNode[F]] extends JCRNodeType[NT] {
  self: NT =>
	override def jcrPrimaryType = "nt:folder"
	
	def subfilesMeta: JCRFileNodeType[F]
 
 /*
 import scala.collection.mutable.{ListBuffer}
 net.liftweb.util.Log.info("RUNSAFE!!!!!!!!!")
   this.runSafe {
    val tArray = new ListBuffer[FieldHolder]

    introspect(this, rootClass.getMethods) {
      case (v, mf) => tArray += FieldHolder(mf.name, v, mf)
    }

    def findPos(in: AnyRef): Box[Int] = {
      tArray.toList.zipWithIndex.filter(mft => in eq mft._1.field) match {
        case Nil => Empty
        case x :: xs => Full(x._2)
      }
    }

    val resArray = new ListBuffer[FieldHolder]

    fieldOrder.foreach(f => findPos(f).foreach(pos => resArray += tArray.remove(pos)))

    tArray.foreach(mft => resArray += mft)

    resArray.toList.foreach { f => net.liftweb.util.Log.info("RESOURCE ----> "+f.name)}
    resArray.toList.foreach { f => net.liftweb.util.Log.info("RESOURCE FIELD ----> "+f.field.name+" --> "+f.field)}
    
    resArray.toList.foreach { f => net.liftweb.util.Log.info("RESOURCE FIELD ----> "+f.field.name+" --> "+f.field)}
    
    var fields__ = resArray.toList map (fh => fh.field)
    fields__.foreach(f => net.liftweb.util.Log.info("FIELDS_TRY ----> "+f.name))
    fields__.flatMap(f => List(f))
    fields__.foreach(f => net.liftweb.util.Log.info("FIELDS ----> "+f.name))
  }

 override def fromJCR(inst:NT, node:Node):Box[NT] = {
     for (val a <- fields(inst)) net.liftweb.util.Log.info("Campi -----> "+a.name)
    //net.liftweb.util.Log.info("Campi -----> "+fields(inst))
    
    Helpers.tryo{
    inst.name(node.getName)
    net.liftweb.util.Log.info("Campi2 -----> "+this.getClass.getName)
    
    for (f <- fields(inst)) {
      f match {
        case prop:JCRProperty[_] => {
          net.liftweb.util.Log.info("Carico la proprieta' "+f.name)
          f.setFromAny(prop.fromJCR(node))
        }
        case child:JCRChildNode[_] => {
          net.liftweb.util.Log.info("Carico il figlio "+f.name)
          f.setFromAny(child.fromJCR(node))
         }
        case e => {
          net.liftweb.util.Log.info("NON meccia!!! "+f.name)
        }
      }
    }
    inst 
  }
   }*/                                  
}

trait JCRFolderNode[T <: JCRFolderNode[T,F], F <: JCRFileNode[F]] extends JCRNode[T] {
  self:T =>
  def meta: JCRFolderNodeType[T,F]
  
  /*object superfolder extends Parent(this,this.meta) {
    override def inverse = Full(subfolder)
  }*/
  object subfolders extends Children(this,this.meta) {
    //override def inverse = Full(superfolder)
  }
  object subfiles extends Children(this,this.meta.subfilesMeta) {}
  
}

trait JCRFileNode[T <: JCRFileNode[T]] extends JCRNode[T] {
  self:T => 
  
  object file extends BinaryProperty(this) {
    private def fileExists(ab: MyType): Box[scala.xml.Node] = {

     if (ab==null || ab.size<1) Full(scala.xml.Text(net.liftweb.http.S.??("file.not.present"))) // mettere il singolo ? quando non faccio piu prove nel boot
     else Empty
    }
    
    override def validators = fileExists _ :: Nil
  }
  
  object mimeType extends StringField(this,32) {
    override def defaultValue = "text/plain"
  }
  
  object encoding extends StringField(this,32) {
    override def defaultValue = "UTF-8"
  }
  
  def setFile(f: File): Unit = setFile(new java.io.FileInputStream(f))
  def setFile(stream: InputStream): Unit = {
    var ba: Array[byte] = new Array(stream.available)
    stream.read(ba)
    file.setFromAny(ba)
  }
  
  def getInputStreamFromFile: InputStream = new ByteArrayInputStream(file.value) // To be tested
  
  def getFile(path: String): File = getFile(new java.io.File(path))
  def getFile(f: File): File = {
    val fos = new java.io.FileOutputStream(f)
    fos.write(file.value)
    fos.flush
    fos.close
    
    f
  }
  
}

class BinaryProperty[OwnerType <: JCRNode[OwnerType]](rec:OwnerType) 
	extends Field[Array[Byte], OwnerType]
 	with JCRProperty[Array[Byte]] 
 	with DBSearchable[OwnerType] {
  def propertyType = PropertyType.BINARY
  def owner = rec
  
  def this(rec: OwnerType, value: Array[Byte]) = {
    this(rec)
    set(value)
  }
  
  override def fromValue(v:Value) = {
    val stream = v.getStream()
    var ret: Array[byte] = new Array(stream.available)
    stream.read(ret)
    stream.close
    ret
  }
  override def asValue(vf:ValueFactory) = {
    val stream = new ByteArrayInputStream(value)
    vf.createValue(stream)
  }
  
  def setFromAny(f: Any): Box[Array[Byte]] = Full(this.set(
    f match {
      case null => Array()
      case arr : Array[Byte] => arr
      case bvi: BinaryValueImpl => fromValue(bvi)
      case _ => null
    }))

  def setFromString(s: String): Box[Array[Byte]] = {
    try{
      Full(set(s.getBytes))
    } catch {
      case e: Exception => Empty
    }
  }
  
  def toForm = scala.xml.NodeSeq.Empty

  def asXHtml: scala.xml.NodeSeq = scala.xml.NodeSeq.Empty

  def defaultValue = Array(0)

  def asJs = null
  
  def getInputStream: InputStream = new ByteArrayInputStream(value)
}