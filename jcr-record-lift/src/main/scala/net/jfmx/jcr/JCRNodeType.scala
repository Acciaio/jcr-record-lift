package net.jfmx.jcr

import _root_.javax.jcr.{Node,Session,Workspace,ValueFactory,PathNotFoundException}
import _root_.scala.collection._
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.record._
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.record.field._
import _root_.scala.collection.mutable.{ListBuffer}
import _root_.net.liftweb.util.Helpers._

/**
 * It represents a NodeType. It is injected inside singleton MetaRecord object to manage persistence 
 * on a Java Content Repository
 */   
 
trait JCRNodeType[NT <: JCRNode[NT]] extends KeyedMetaRecord[String,NT] with JCRItem {
  self: NT =>
  
  /* to remove duplicates */
  override def introspect(rec: NT, methods: Array[Method])(f: (Method, OwnedField[NT]) => Any) = 
    {
      val fs = if (!fields.isEmpty) fields.map(f => f.name)
               else Nil
	  val m = new ListBuffer[Method]
	  for (val met <- methods) {
		  if (m.isEmpty || ((m.find(c => ((c.getName==met.getName) ||
			((fs.isEmpty || !fs.contains(met.getName)) && 
			((classOf[Field[_, _]].isAssignableFrom(met.getReturnType)) &&
			tryo{c.invoke(rec)==met.invoke(rec)}==Full(true)))))).isEmpty)) {
			m += met
		  }
	  }
	  super.introspect(rec,m.toArray)(f)
  }
  
  def initRoot:Unit = initRoot(jcrDefaultSessionIdentifier)
  def initRoot(sId:SessionIdentifier):Unit = JCR.use(sId) {
    session =>  
    val root = session.getRootNode().getNode(sId.appRoot)
    if (!root.hasNode(jcrName)) {
      root.addNode(jcrName,jcrRootPrimaryType)
      session.save
    }
  }
  
  def jcrRootPrimaryType = "nt:unstructured"
  def jcrPrimaryType = "nt:unstructured"  //This version will be schemaless
  var jcrMixinTypes:List[String] = Nil    //Add versionable and referenceable for versionable and referenceable nodes
  /*override def*/ jcrName/*:String*/ = rootClass.getName.split("\\.").toList.last // Nome della classe dal punto di vista dell'applicazione
  def jcrDefaultSessionIdentifier = DefaultSessionIdentifier
  
  /**
   * This is the path of the node to which attach new saved items
   */
  def rootPath(sId: SessionIdentifier):String = sId.appRoot+"/"+jcrName 
  
  override def createRecord = {
    val nt = JCRNodeType.getRealNodeType(this)
    if (this == nt) super.createRecord
    else nt.createRecord
  }
   
  override def createRecord(json:String) = createRecord.fromJSON(json)
  
  def create(node:Node):NT = createRecord.fromJCR(node).open_!
  
  import JCRExtensions._
  
  def findAll(sId:SessionIdentifier, by: QueryParam[NT]*): List[NT]  = Nil
    
  def findAll(sId:SessionIdentifier, queryString:String):List[NT] = JCR.xPathQuery(sId,queryString).map {
    node =>
    create(node).sessionIdentifier(sId)
  }.toList  
    
  
  def findAll(sId:SessionIdentifier):List[NT] = JCR.use(sId) { 
    session =>
    val root = session.getRootNode().getNode(sId.appRoot).getNode(jcrName)
    root.getNodes().map { node => {
      create(node).sessionIdentifier(jcrDefaultSessionIdentifier)}  
    }.toList
  }
    
    
  def find(sId:SessionIdentifier, aName:String):Box[NT] =  JCR.use(sId) {
    session => Helpers.tryo{
      val root = session.getRootNode().getNode(sId.appRoot).getNode(jcrName)
      create(root.getNode(aName)).sessionIdentifier(sId)
    } 
  }
  
  def exists(sId:SessionIdentifier,aName:String):Boolean = JCR.use(sId) {
    session => Helpers.tryo{
      val root = session.getRootNode().getNode(sId.appRoot).getNode(rootPath(sId))
      root.hasNode(aName)
    }.open_!
  }
  
  def findAll() = findAll(jcrDefaultSessionIdentifier)
  def findAll(by: QueryParam[NT]*): List[NT] = findAll(jcrDefaultSessionIdentifier, by: _*)
  def find(aName:String) = find(jcrDefaultSessionIdentifier, aName) 
  def exists(aName:String) = exists(jcrDefaultSessionIdentifier,aName)
  
  def save(toSave:NT):Boolean = {
    if ((toSave.jcrName=="") || (toSave.jcrName==null)) {
      toSave.jcrName=jcrName+"/"+toSave.name.value
      saveAt(jcrName, toSave)
    }
    else {
      saveAt(toSave.jcrName.split("\\/").toList.tail.tail.reverse.tail.reverse.mkString("","/",""), 
    		 toSave)//saveAt(jcrName, toSave)
    }
  }
  
  def saveAt(parentPath:String, toSave:NT): Boolean = 
    if (toSave==this || (toSave.jcrName=="") || (saved_?(toSave) && clean_?(toSave))) true 
    else {
      JCR.use(toSave.sessionIdentifier) {
        session => 
        //_beforeSave(toSave)
        val parent = session.getRootNode().getNode(toSave.sessionIdentifier.appRoot).getNode(parentPath)
        try {
          val node = parent.getNode(toSave.name.value)
          //_beforeUpdate(toSave)
          asJCR(session,toSave,node)
        } catch {
          // da sistemare
          case a:PathNotFoundException =>
            //beforeCreate(toSave) 
            asJCR(session,toSave,parent.addNode(toSave.name.value,jcrPrimaryType))
            //afterCreate(toSave)
          case _ => return false
        }  
        session.save
      }
      true
    }
       
  def delete_!(inst:NT) = JCR.use(inst.sessionIdentifier){
    session =>
    val root = session.getRootNode()//.getNode(toSave.sessionIdentifier.appRoot).getNode(inst.jcrName)//getNode(inst.sessionIdentifier.appRoot).getNode(jcrName)
    //net.liftweb.util.Log.info(inst.jcrName.split("\\/").toList.tail.mkString("","/",""))
    val node = root.getNode(inst.jcrName.split("\\/").toList.tail.mkString("","/",""))
    node.remove
    session.save
    true
  }
  
  def fromJCR(inst:NT, node:Node):Box[NT] = Helpers.tryo{
    inst.name(node.getName)
    inst.jcrName = node.getPath
    for (f <- fields(inst)) {
      f match {
        case prop:JCRProperty[_] => f.setFromAny(prop.fromJCR(node))
        case child:JCRChildNode[_] => {
          //net.liftweb.util.Log.info("jcrName : "+inst.jcrName+" name "+inst.name)
          val path = inst.jcrName+"/"+f.name+"/"
          f.setFromAny(child.fromJCR(node,path))
        }
        case e => {}
      }
    }
    inst 
  }
  
  def asJCR(session:Session, inst:NT, node:Node):Node = {
    for (f <- fields(inst)) { 
      f match { 
        case prop:JCRProperty[_] => prop.asJCR(session,node) 
        case child:JCRChildNode[_] => child.asJCR(session,node)  
        case e => {}
      }
    }
    node
  }
  
  def saved_?(inst:NT):Boolean = false
  def dirty_?(inst:NT):Boolean = false
  def clean_?(inst:NT):Boolean = fields(inst).foldLeft(true){(bool , f) => bool && !f.dirty_?}
  
  def find(start: Box[Int], max: Box[Int],
           lop: LOp,by: QueryParam[NT]*): List[NT] = find(jcrDefaultSessionIdentifier, start, max, lop, by: _*) 
  def find(si: SessionIdentifier,start: Box[Int], max: Box[Int],
                        lop: LOp,by: QueryParam[NT]*): List[NT] = {
    def prefix = "/jcr:root/"+si.appRoot+"/"+jcrName+"/element(*, nt:unstructured)"
    
    JCR.use(si) {
      conn =>
      
      println(composeQuery(prefix,lop,by.toList))
      val res = JCR.xPathQuery(si,composeQuery(prefix,lop,by.toList)).toList
      val resWithStart = 
    	(start) match {
        	case Full(sta) =>  res.takeRight(sta)
        	case _ => res 
      	}
      val resWithStartAndMax =
        (max) match {
          case Full(m) => resWithStart.dropRight(m)
          case _ => resWithStart
        }
      
      resWithStartAndMax.map(n => {
        //println(n)
        create(n)
      })
    }
  }
  
  private def composeQuery(prefix: String,lop: LOp,by: List[QueryParam[NT]]): String = {
      //Prima divido i criteri di ordinamento da quelli di selezione
      prefix+"["+composeParams(lop,by)+"] "+composeOrder(by)
  }
  
  private def composeParams(lop: LOp,by: List[QueryParam[NT]]): String = {
    val resParts = 
    by.map(q => 
      q match {
            case Cmp(field: DBSearchable[NT], opr, Full(s), _) => 
              (s) match {
                case _ : String => "("+field.dbSelectString+" "+opr+" '"+s+"')"
                case _ => "("+field.dbSelectString+" "+opr+" "+s+")"
              } 
            case Cmp(field: DBSearchable[NT], opr, _, Full(otherField: DBSearchable[NT])) =>
                "("+field.dbSelectString+" "+opr+" "+otherField.dbSelectString+")"
            case _ =>
          })
    
     (lop) match {
      	case And => resParts.mkString(""," and ","")
      	case Or => resParts.mkString(""," or ","")
      	case _ => ""
      }
  }
  
  private def composeOrder(by: List[QueryParam[NT]]): String = {
    val ord = 
      by.flatMap( q => 
    	q match {
    	  case OrderBy(field: DBSearchable[NT],order:AscOrDesc) =>
                Some(field.dbSelectString+" "+order.xPath)
    	  case _ => None
    	}
      )
    
    (ord) match {
      case Nil => ""
      case _ => "order by"+ord.mkString(" ",", ","")
    }
  }
  
}


/**
 * It implements a simple inheritance mechanisms.
 * If C is a JCRNode and C' is a subclass of C, it order to use C' in an application,
 * it is sufficient to register C' JCRNodeType with the same name of C inside the nodeType map.
 * This limited form of inheritance permits specialization of key model object inside an application.
 * A modified application could adapt to limited schema modification and use a previously populated repo.
*/
object JCRNodeType { 
  
  LiftRules.addToPackages("net.jfmx.jcr")
  
  private val map = new mutable.HashMap[String,JCRNodeType[_]]
  def registerNodeType/*[T <: JCRNode[T]]*/(meta:JCRNodeType[_]) =  map(meta.jcrName) = meta
  
  def registerAndInitNodeTypes/*[T <: JCRNode[T]]*/(metas:JCRNodeType[_]*) = {
    metas.foreach{ meta => {
    					registerNodeType(meta)
    					meta.initRoot
                   }
    }
  }
  
  def getRealNodeType[T <: JCRNode[T]](superNT:JCRNodeType[T]):JCRNodeType[T] = map.get(superNT.jcrName) match {
    case None => superNT
    case Some(nt) => nt.asInstanceOf[JCRNodeType[T]]
  }
  
  def getNodeType[T <: JCRNode[T]](name:String):Box[JCRNodeType[T]] = map.get(name) match {
    case None => Empty
    case Some(nt) => Full(nt.asInstanceOf[JCRNodeType[T]])
  }
}


