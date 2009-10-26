package net.jfmx.jcr

import _root_.javax.jcr.{Repository,Session,Node,NodeIterator,Property,PropertyIterator,SimpleCredentials,Credentials}
import _root_.javax.naming.{Context, InitialContext}
import _root_.scala.collection.mutable
import _root_.org.apache.jackrabbit.core.{TransientRepository,RepositoryImpl}
import _root_.org.apache.jackrabbit.core.config.{RepositoryConfig}
import _root_.org.apache.jackrabbit.rmi.client.{ClientRepositoryFactory}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{RequestVar}
import _root_.scala.actors.{Actor}
import _root_.scala.actors.Actor._

object JCRExtensions {
  implicit def nodeIterator(it:NodeIterator):Iterator[Node] = new Iterator[Node] {
      def hasNext = it.hasNext
      def next = it.nextNode
  }
  
  implicit def propertiesIterator(it:PropertyIterator):Iterator[Property] = new Iterator[Property] {
      def hasNext = it.hasNext
      def next    = it.nextProperty
  }
    
}

object JCR {
  private val threadStore = new ThreadLocal[mutable.HashMap[SessionIdentifier, SessionHolder]]
  private val envContext  = FatLazy((new InitialContext).lookup("java:/comp/env").asInstanceOf[Context])
  
  var queryTimeout:Box[Int] = Empty
  
  private var logFuncs: List[(String,Long)=>Any] = Nil
  
  def addLogFunc(f:(String,Long) => Any):List[(String,Long) => Any] = {
    logFuncs = f :: logFuncs
    logFuncs
  }
  
  /**
   * Can we have access to the repository from JNDI ?
   */
  def jndiRepoAvailable_? : Boolean = {
    val touchedEnv = envContext.calculated_?
    
    val ret = try {
      (envContext.get.lookup(DefaultSessionIdentifier.jndiName).asInstanceOf[Repository]) != null
    } catch {
      case e => false
    }
    if (!touchedEnv) envContext.reset
    ret
  }
  
  //var sessionManager: Box[SessionManager]  = Empty
  private val sessionManagers = new mutable.HashMap[SessionIdentifier, SessionManager]
  
  def createSessionManager(name: String,configFile: String,homeDir: String): SessionManager = {
    try {
      Repo.init(configFile,homeDir)
    } catch {
      case _ => {
        SessionReleaser.closeAll
        Repo.shutdown
        createSessionManager(name,configFile,homeDir)
      }
    }
    Repo
  }
  
  def createSessionManagerRMI(url: String , appName: String/*, workspace: String*/): SessionManager = {
    try {
      RMIRepo.init(url,appName/*,workspace*/)
    } catch {
      case _ => {
        SessionReleaser.closeAll
        RMIRepo.shutdown
        createSessionManagerRMI(url,appName/*,workspace*/)
      }
    }
    RMIRepo
  }
  
  def defineSessionManager(name:SessionIdentifier, mgr:SessionManager) {
    sessionManagers(name) = mgr
  }
  
  case class SessionHolder(session:SuperSession, cnt:Int, postCommit:List[() => Unit])
  
  private def info: mutable.HashMap[SessionIdentifier, SessionHolder] = {
    threadStore.get match {
      case null => 
        val tinfo = new mutable.HashMap[SessionIdentifier, SessionHolder]
        threadStore.set(tinfo)
        tinfo
      case v => v
    }
  }
  
  // remove thread-local association
  def clearThread:Unit = {
    val i = info
    val ks = i.keySet
    if (ks.isEmpty) threadStore.remove
    else {
      ks.foreach(n => releaseSessionNamed(n))
      clearThread
    }
  }
   
  private def newSession(name:SessionIdentifier) : SuperSession = {
    val ret = (Box(sessionManagers.get(name)).flatMap(sm => sm.newSession(name).map(s => new SuperSession(s,()=> sm.releaseSession(s))))) openOr {
      Helpers.tryo {
        val uniqueId = if (Log.isDebugEnabled) Helpers.nextNum.toString else ""
        Log.debug("Session ID " + uniqueId + " for " + name + " opened" )
        val session = envContext.get.lookup(name.jndiName).asInstanceOf[Repository].login(name.wsName)
        new SuperSession(session,()=>{Log.debug("Session ID "+uniqueId+" for "+ name + " closed"); session.logout})  
      } openOr { throw new NullPointerException("Looking for "+name+" but failed to find either a JNDI Repository"+
                 "with the name "+name+" or a session manager with the correct name")}
    }
    //ret.setAutoCommit(false)
    ret
  }
  
  /**
   * Build a Loanwrapper to pass into S.addAround to make requests for
   * the list of SessionIdentifiers transactional for the complete HTTP request
   */
  def buildLoanWrapper(in:List[SessionIdentifier]):LoanWrapper = new LoanWrapper {
      private def doWith[T](in:List[SessionIdentifier],f: => T):T = in match {
        case Nil => f
        case x :: xs => use(x)(ignore => doWith(xs,f))
      }
      private object DepthCnt extends RequestVar(0)
      def apply[T](f: => T):T = try {
        DepthCnt.update(_ + 1)
        doWith(in, f)
      } finally {
        DepthCnt.update(_ - 1)
        if (DepthCnt.is == 0) clearThread  
      }
  }  
    
  private def releaseSession(session:SuperSession):Unit = session.logout()
  
  private def getSession(name:SessionIdentifier): SuperSession = {
    Log.trace("Acquiring session "+ name + " on thread "+ Thread.currentThread)
    var ret = info.get(name) match {
      case None => SessionHolder(newSession(name),1,Nil)
      case Some(SessionHolder(sess,cnt,post)) => SessionHolder(sess,cnt+1,post)
    }
    info(name) = ret
    Log.trace("Acquired connection "+ name + " on thread " + Thread.currentThread + " count " + ret.cnt)
    ret.session
  }
  
  /*private*/ def releaseSessionNamed(name:SessionIdentifier) {
    Log.trace("Request to release session: "+name+" on thread "+Thread.currentThread)
    (info.get(name): @unchecked) match {
      case Some(SessionHolder(s,1,post)) => 
        s.save
        Helpers.tryo(s.releaseFunc())
        info -= name
        post.reverse.foreach(f => Helpers.tryo(f()))
        Log.trace("Released session "+name+" on thread "+Thread.currentThread)
      case Some(SessionHolder(s,n,post)) =>
        Log.trace("Did not release session "+name+" on thread "+Thread.currentThread+ " count "+ (n-1))
        info(name) = SessionHolder(s,n-1,post)
      case _ => 
        // ignore  
    }
  }
  
  /**
   * Append a function to be invoked before releasing a session 
   */
  def appendPostFunc(name:SessionIdentifier, func:() => Unit) {
    info.get(name) match {
      case Some(SessionHolder(s,n,post)) => info(name) = SessionHolder(s,n,func :: post)
      case _ => 
    }
  }
  
  private def runLogger(query:String, time:Long) {
    logFuncs.foreach(_(query,time))
  }
  
  /** 
   * Executes function {@code f} with the session named {@code name}. Releases the session 
   * before returning. 
   */  
  def use[T](name :SessionIdentifier)(f : (SuperSession) => T) : T = {
	SessionReleaser.reset(name)
      val sess = try {
        getSession(name)
      } catch {case _ => clearThread; null}
    try {
      f(sess)  

    } finally {
      //sess.save TO VERIFY
      //releaseSessionNamed(name)
    }
  }
  SessionReleaser.start

  
  import JCRExtensions._
  import javax.jcr.query.{Query}
  
  def query(sId:SessionIdentifier,statement:String,xlang:String):Iterator[Node] = JCR.use(sId) { 
    session =>
      val qm = session.getWorkspace().getQueryManager()
      val q = qm.createQuery(statement,xlang)
      val qr = q.execute()
      qr.getNodes()
  }
  
  def xPathQuery(sId:SessionIdentifier, xpath:String) = query(sId,xpath,Query.XPATH)
  def sqlQuery(sId:SessionIdentifier, stmt:String) = 	query(sId,stmt,Query.SQL)
  
  def exportSystemView(sId:SessionIdentifier, filePath:String, absPath: String) = 
    JCR.use(sId) {
	  session =>
        val file = new java.io.File(filePath) 
        if (!file.exists) file.createNewFile
        val fo=new java.io.FileOutputStream(file,true)
        session.exportSystemView("/"+absPath,  fo, false,false) // deve diventare la root del progetto
        fo.close
  	}
  def exportDocumentView(sId:SessionIdentifier, filePath:String, absPath: String) = 
    JCR.use(sId) {
	  session =>
        val file = new java.io.File(filePath) 
        if (!file.exists) file.createNewFile
        val fo=new java.io.FileOutputStream(file,true)
        session.exportDocumentView("/"+absPath,  fo, false,false) // deve diventare la root del progetto
        fo.close
  	}
  def importXML(sId: SessionIdentifier, filePath: String, absPath: String) = 
	  JCR.use(sId) {
	  	session =>
	  		try {
	  		  val file = new java.io.File(filePath)
	  		  val fi = new java.io.FileInputStream(file)
	  		  session.importXML("/"+absPath,fi,javax.jcr.ImportUUIDBehavior.IMPORT_UUID_COLLISION_REPLACE_EXISTING)
	  		  fi.close
	  		  session.save
	  		} catch {
	  		  case e => e.printStackTrace
	  		}
	  }
  
}

class SuperSession(val session:Session, val releaseFunc: () => Any) {
  
}
  
object SuperSession {  
  implicit def superToSession(in: SuperSession): Session = in.session  
}  
  
trait SessionIdentifier {  
  def jndiName: String 
  def wsName:String
  def user:String
  def passwd:String
  
  def appRoot:String
  override def toString() = "SessionIdentifier("+jndiName+"/"+wsName+")"  
  override def hashCode() = jndiName.hashCode()  
  override def equals(other: Any): Boolean = other match {  
    case ci: SessionIdentifier => (ci.jndiName == this.jndiName) && (ci.wsName == this.wsName)  
    case _ => false  
  }  
}  

object SessionReleaser extends java.util.TimerTask {
    var sessions: scala.collection.mutable.Map[SessionIdentifier,Int] = scala.collection.mutable.Map()
    val defaultTimeout = 10
    
    def reset(name: SessionIdentifier) = (sessions.get(name)) match {
      case Some(_) => sessions.update(name,10)
      case _ => sessions += (name -> 10)
    }
    
    val timer= new java.util.Timer(true)
    
    def start = timer.schedule(this,new java.util.Date,1000)
    
    def isEmpty_? = sessions.isEmpty
    
    def closeAll = sessions.foreach{s => {
      JCR.releaseSessionNamed(s._1) 
      sessions.removeKey(s._1)
    }}
    
    def run =
          	try {
  	      		sessions.foreach{ s => {
  	      			try {
  	      				if (s._2<=0) { 
  	      					Log.info("Closing session "+s._1)
  	      					JCR.releaseSessionNamed(s._1)
  	      					sessions.removeKey(s._1)
  	      				} else
  	      			sessions.update(s._1,s._2-1)
  	      			} catch { case _ => {}}
  	      		}}
  	      	} catch {case _ => {this.cancel}}

  }
  
case object DefaultSessionIdentifier extends SessionIdentifier {  
  var jndiName = "jfmx" 
  var wsName = "default"
  var user = "admin" // o system e vuoto?
  var passwd = "admin"
  
  var appRoot = "root"
  
  def setAppRoot(x: String) = appRoot = x
}

trait SessionManager {
  def newSession(name:SessionIdentifier):Box[Session]
  def releaseSession(session:Session)
  
}

object TransientRepo extends SessionManager {
  val repo = new TransientRepository()
  def newSession(name:SessionIdentifier) = {
    try {
      SessionReleaser.reset(name)
      Full(repo.login(
        	new SimpleCredentials(name.user,name.passwd.toCharArray),name.wsName))
    } catch {
      case e : Exception => /*e.printStackTrace;*/ Empty
    }
  }
  def releaseSession(session:Session) {
    session.logout
  }
}

object Repo extends SessionManager {
 
  var repo: RepositoryImpl = null
  var name: String = null
  
  def init(configFile: String ,homeDir: String): Unit = {
	try {
		repo = RepositoryImpl.create(RepositoryConfig.create(configFile,homeDir))
	} catch {
	  case _ => {
	    Log.info("Try to remove the lock file if this is a restart")
	    val rm = RepositoryConfig.create(configFile,homeDir).getRepositoryLockMechanism
	    rm.init(homeDir)
	    rm.release
	    init(configFile,homeDir)
	  }
	}
  }
  
  def shutdown = {
    if (repo!=null) {
    	repo.shutdown
    	repo = null
    }
  }
  
  def newSession(name:SessionIdentifier) = {
    try {
      SessionReleaser.reset(name)
      val session = repo.login(
        	new SimpleCredentials(name.user,name.passwd.toCharArray),name.wsName) // verificare che accetta le credenziali di default
      val ws = session.getWorkspace
      val rn = session.getRootNode
      if (!rn.hasNode(name.appRoot)) {
    	  rn.addNode(name.appRoot,"nt:unstructured")
    	  session.save
      }
      Full(session)
    } catch {
      case e : Exception => /*e.printStackTrace;*/ Empty
    }
  }
  def releaseSession(session:Session) {
    session.logout
  }

}

object RMIRepo extends SessionManager {
     
  val repoFactory = new ClientRepositoryFactory
  var name: String = null
  
  def init(url: String , appName: String): Unit = {
		name = url+appName
  }
  
  def shutdown = {
    	name = null
  }
  
  def newSession(si:SessionIdentifier) = {
    try {
      SessionReleaser.reset(si)
      val repo = repoFactory.getRepository(name)
      val session = repo.login(
        	new SimpleCredentials(si.user,si.passwd.toCharArray),si.wsName)
      val ws = session.getWorkspace
      val rn = session.getRootNode
      if (!rn.hasNode(si.appRoot)) {
    	  rn.addNode(si.appRoot,"nt:unstructured")
    	  session.save
      }
      Full(session)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseSession(session:Session) {
    session.logout
  }

}

class JCRException(msg:String) extends Exception
