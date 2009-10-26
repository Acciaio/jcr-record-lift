package net.liftweb.record

import sitemap._
import Loc._
import http._
import js._
import util._
import _root_.net.liftweb._
import _root_.scala.xml._
import net.liftweb.record._
import _root_.net.liftweb.common._
import Helpers._

trait CRUDel /*ements*/[KeyType,CrudType <: KeyedRecord[CrudType,KeyType]] {
  self: CrudType with KeyedMetaRecord[KeyType,CrudType] =>

  lazy val Prefix = calcPrefix
  lazy val ListItems = calcListItems
  lazy val ViewItem = calcViewItem
  lazy val CreateItem = calcCreateItem
  lazy val EditItem = calcEditItem
  lazy val DeleteItem = calcDeleteItem

  def calcPrefix = List(internalClassName)
  def calcListItems = "list"
  def calcViewItem = "view"
  def calcCreateItem = "create"
  def calcEditItem = "edit"
  def calcDeleteItem = "delete"
  
  lazy val listPath = Prefix ::: List(ListItems)
  lazy val listPathString: String = mp(listPath)
  lazy val createPath = Prefix ::: List(CreateItem)
  lazy val createPathString: String = mp(createPath)
  lazy val viewPath = Prefix ::: List(ViewItem)
  lazy val viewPathString: String = mp(viewPath)
  lazy val editPath = Prefix ::: List(EditItem)
  lazy val editPathString: String = mp(editPath)
  lazy val deletePath = Prefix ::: List(DeleteItem)
  lazy val deletePathString: String = mp(deletePath)
  
  private def mp(in: List[String]) = in.mkString("/", "/", "")

  def displayName = displayClassName

  def displayHtml: NodeSeq = Text(calcPrefix.head)
  
  def obscurePrimaryKey(in: CrudType): String = obscurePrimaryKey(in.primaryKey.toString)
  def obscurePrimaryKey(in: String): String = in
  
  def fromStringToKey(s: String): Box[KeyType] = tryo{s.asInstanceOf[KeyType]}
  
  def findViewForParam(in: String): Box[CrudType] = 
	  fromStringToKey(in) match {
	  	case Full(key) => find(key)
	  	case _ => Empty
  	  }
  
  def menus: List[Menu] =
	  List(showAllMenuLoc, viewMenuLoc, createMenuLoc,
		   editMenuLoc).flatMap(x => x)
  
  def pageWrapper(body: NodeSeq): NodeSeq =
	  <lift:surround with="default" at="content">
    	{
    		body
    	}
      </lift:surround>

  def viewTemplate(): NodeSeq = pageWrapper(_viewTemplate)
      
  def viewId = "view_page"
  def viewClass = "view_class"
    
  def _viewTemplate =
  <lift:crud.view form="POST">
    <crud:menus/>
    <table id={viewId} class={viewClass}>
      <crud:row>
        <tr>
          <td><crud:name/></td>
          <td><crud:value/></td>
        </tr>
      </crud:row>
    </table>
    <crud:buttons/>
  </lift:crud.view>
      
  def getFieldsNames( l: List[OwnedField[CrudType]]) = 
      l.flatMap(f => if (!(f.canRead_?)) Nil
                    else Some(f.name))
    
  def getFieldsFromNames(l: List[String],entry: CrudType) = 
	  l.flatMap(name => {(fieldByName(name,entry)) match {
	  						case Full(field) => List(field)
    						case _ => Empty
                       }})

  def orderedViewFieldsItems(entry: CrudType) = fields(entry)
  def orderedViewFieldsNames(entry: CrudType) = getFieldsNames(orderedViewFieldsItems(entry))
  private def orderedViewFields(entry: CrudType) = getFieldsFromNames(orderedViewFieldsNames(entry),entry) 

  def viewMenuLoc: Box[Menu] =
  Full(Menu(new Loc[CrudType]{
        // the name of the page
    
        def name = S.??("View")+Prefix

        override val snippets: SnippetTest = {
          case ("crud.view", Full(wp)) => displayRecord(wp) _
        }
        def defaultParams = Empty
        def params = viewMenuLocParams

        /**
         * What's the text of the link?
         */
        val text = new Loc.LinkText(calcLinkText _)
        def calcLinkText(in: CrudType): NodeSeq = Text("Edit")

        /**
         * Rewrite the request and emit the type-safe parameter
         */
        override val rewrite: LocRewrite =
        Full(NamedPF(name) {
            case RewriteRequest(pp , _, _)
              if (pp.wholePath.startsWith(viewPath) &&
              pp.wholePath.length == (viewPath.length + 1) &&
              findViewForParam(pp.wholePath.last).isDefined)
              =>
              (RewriteResponse(viewPath),findViewForParam(pp.wholePath.last).open_!)
          })

        def displayRecord(entry: CrudType)(in: NodeSeq): NodeSeq = {
          
          def doRow(in: NodeSeq): NodeSeq =
          orderedViewFields(entry).flatMap(
            f => bind("crud", in, "name" -> f.displayName, "value" -> f.toXHtml)
          )
             
          bind("crud", in, "row" -> doRow _,
          "menus" -> viewUpMenus(entry),
          "buttons" -> viewButtons(entry))
        }

        override def calcTemplate = Full(viewTemplate)

        val link =
        new Loc.Link[CrudType](viewPath, false) {
          override def createLink(in: CrudType) =
          Full(Text(viewPathString+"/"+obscurePrimaryKey(in)))
        }
      }))
  
  def editButton(item: CrudType) =
    SHtml.submit(editName, () => goToEdit(item)) 
  
  def goToEdit(item: CrudType) = 
    S.redirectTo(editPathString+"/"+obscurePrimaryKey(item))
  
  def buttonsId = "buttons_table"
  def buttonsClass = "buttons_class"
  
  def menusId = "buttons_table"
  def menusClass = "buttons_class"
  
  def upMenusTemplate(in : NodeSeq) = 
    <table id={menusId} class={menusClass}>
    	{in}
    </table>
    
  def buttonsTemplate(in : NodeSeq) = 
    <table id={buttonsId} class={buttonsClass}>
    	{in}
    </table>
  
  def viewUpMenus(item: CrudType) = 
	  upMenusTemplate({
    	<tr>
    		<td>
    			<a href={listPathString}>{S.?("List")+" "+displayName}</a>
    		</td>
        </tr>})
      
  def viewButtons(item: CrudType) =
    buttonsTemplate({
       <tr>
           <td>{editButton(item)}</td>
           <td>{deleteButton(item)}</td>
       </tr>})
  
  /**
   * Override to include new Params for the view menu
   */
  def viewMenuLocParams: List[Loc.LocParam] = Nil
  
  def editMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[CrudType]{
          // the name of the page
          def name = "Edit "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.edit", Full(wp)) => crudDoForm(wp, wp => editedMessage(wp),item => orderedEditFields(item))
          }
          def defaultParams = Empty
          def params = editMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)
          def calcLinkText(in: CrudType): NodeSeq = Text("Edit")

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
              case RewriteRequest(pp , _, _)
                if pp.wholePath.startsWith(editPath) &&
                pp.wholePath.length == (editPath.length + 1) &&
                findViewForParam(pp.wholePath.last).isDefined
                =>
                (RewriteResponse(editPath),findViewForParam(pp.wholePath.last).open_!)
            })

          override def calcTemplate = Full(editTemplate)

          val link =
          new Loc.Link[CrudType](editPath, false) {
            override def createLink(in: CrudType) =
            Full(Text(editPathString+"/"+obscurePrimaryKey(in)))
          }
        }))
  }
  
  def editedMessage(item: CrudType) = noticeMessage(Text{S.?("Edited")+" "+item.primaryKey})

  /**
   * Override to include new Params for the edit menu
   */
  def editMenuLocParams: List[Loc.LocParam] = Nil


  def editMenuName = S.??("Edit")+" "+displayName
  
  def orderedEditFieldsItems(entry: CrudType) = fields(entry)
  def orderedEditFieldsNames(entry: CrudType) = getFieldsNames(orderedEditFieldsItems(entry))-primaryKey.name
  private def orderedEditFields(entry: CrudType) = getFieldsFromNames(orderedEditFieldsNames(entry),entry)
  
  def editTemplate(): NodeSeq = pageWrapper(_editTemplate)

  def editId = "edit_page"
  def editClass = "edit_class"

  def editUpMenus(item: CrudType): NodeSeq =
    upMenusTemplate({
    	<tr>
    		<td>
    			<a href={listPathString}>{S.?("List")+" "+displayName}</a>
    		</td>
    		<td>
    			<a href={viewPathString+"/"+obscurePrimaryKey(item)}>{S.?("View")}</a>
    		</td>
        </tr>})
  
  def editButtons(item: CrudType): NodeSeq =
	  buttonsTemplate({
       	<tr>
           <td>
           <lift:crud.edit>
             <crud:submit>{editName}</crud:submit>
           </lift:crud.edit>
           </td>
           <td>{deleteButton(item)}</td>
       </tr>})
    
  def _editTemplate =
  <lift:crud.edit form="POST">
    <table id={editId} class={editClass}>
      <crud:editMenus/>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>
    </table>
    <crud:editButtons/>
  </lift:crud.edit>

  def editName = S.?("Edit")

  def deleteButton(item: CrudType) =
    SHtml.submit(deleteName, () => doDelete(item))
  
  def doDelete(item: CrudType) = {
    delete_!(item)
    S.notice(deletedMessage(item))
    S.redirectTo(listPathString)
  }
  
  def deletedMessage(item: CrudType) = noticeMessage(Text(S.?("Deleted")+" "+item.primaryKey))
  
  def deleteName = S.?("Delete")
  
  def createMenuLoc: Box[Menu] =
  Full(Menu(Loc("Create "+Prefix, createPath, createMenuName,
                locSnippets :: Loc.Template(createTemplate) :: createMenuLocParams)))

  def createMenuLocParams: List[Loc.LocParam] = Nil
  
  
  def createMenuName = S.?("Create")+" "+displayName

  def orderedCreateFieldsItems(entry: CrudType) = fields(entry)
  def orderedCreateFieldsNames(entry: CrudType) = getFieldsNames(orderedCreateFieldsItems(entry))
  private def orderedCreateFields(entry: CrudType) = getFieldsFromNames(orderedCreateFieldsNames(entry),entry)
 
  def createTemplate(): NodeSeq = pageWrapper(_createTemplate)

  def createId = "create_page"
  def createClass = "create_class"
  
  def createName = S.?("Create")
  
  def createUpMenus(item: CrudType): NodeSeq =
    upMenusTemplate({
    	<tr>
    		<td>
    			<a href={listPathString}>{S.?("List")+" "+displayName}</a>
    		</td>
        </tr>})
  
  def createButtons(item: CrudType): NodeSeq =
	buttonsTemplate({
       	<tr>
           <td>
           <lift:crud.create>
             <crud:submit>{createName}</crud:submit>
           </lift:crud.create>
           </td>
       </tr>})

  def _createTemplate =
    <lift:crud.create form="POST">
    <table id={createId} class={createClass}>
      <crud:createMenus/>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>
    </table>
    <crud:createButtons/>
  </lift:crud.create>
  
      
  def createButton = S.??("Create")
  
  def createdMessage(item: CrudType) = noticeMessage(Text{S.?("Created")+" "+item.primaryKey})
  
  def referer: String = S.referer openOr listPathString
  
  def crudDoForm(item: CrudType, calcNoticeMsg: Function1[CrudType,NodeSeq], calcOrder: Function1[CrudType,List[OwnedField[CrudType]]])(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet
    
    def loop(html:NodeSeq): NodeSeq = {
      def doFields(html: NodeSeq): NodeSeq =
      calcOrder.apply(item).flatMap(f =>
        bind("crud", html, "name" -> f.displayName, "form" -> f.asXHtml))

      def doSubmit() = item.validate match {
        case Nil => {
              S.notice(calcNoticeMsg.apply(item))
              save(item)
              S.redirectTo(from)
        }
        case xs =>
          S.error(errorMessage(xs))
          snipName.foreach(S.mapSnippet(_, loop))
      }

      bind("crud", html,
           "field" -> doFields _,
           "submit" -> ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)),
      	   "editMenus" -> editUpMenus(item),
      	   "editButtons" -> editButtons(item),
      	   "createMenus" -> createUpMenus(item),
      	   "createButtons" -> createButtons(item))
    }

    loop(in)
  }
   
  def errorMessage(xs: List[FieldError]): NodeSeq = 
    <div>
       <span color="red">Text{S.?("Error")}</span>
       <ul>
       	{for (val e <- xs) yield (<li>{e.toString}</li>)}
       </ul>
    </div>
  
  def noticeMessage(msg: NodeSeq): NodeSeq = msg 
    
  def showAllMenuLoc: Box[Menu] =
  Full(Menu(Loc("List "+Prefix, listPath, showAllMenuName,
                locSnippets :: Loc.Template(showAllTemplate) :: showAllMenuLocParams)))

  def showAllMenuLocParams: List[Loc.LocParam] = Nil
  
  def showAllMenuName = S.?("List")+" "+displayName

  def showAllTemplate(): NodeSeq = pageWrapper(_showAllTemplate)

  def showAllId = "show_all"
  def showAllClass = "show_all"
  
  def rowsPerPage: Long = 10
  
  def lineAsLink: Boolean = true
  
  def fieldsNotSortable: List[OwnedField[CrudType]] = Nil
  
  /* List to return contains Field, BasePath to link to and boolean 
   * that indicates if you must add the primaryKey or not to the link
   */
  def fieldsLinkable: List[(OwnedField[CrudType],String,Boolean)] =  
    orderedListFieldsItems(this).map(f =>
    	(f,{viewPathString+"/"},true))
  
  def fieldsSearchable: List[OwnedField[CrudType]] = orderedListFieldsItems(this)
  
  def orderedListFieldsItems(entry: CrudType) = fields(entry)
  def orderedListFieldsNames(entry: CrudType) = getFieldsNames(orderedListFieldsItems(entry))
  private def orderedListFields(entry: CrudType) = getFieldsFromNames(orderedListFieldsNames(entry),entry)
  
  def listUpMenus = {
    upMenusTemplate({
    	<tr>
    		<td>
    			<a href={createPathString}>{S.?("Create")+" "+displayName}</a>
    		</td>
        </tr>})
  }
  
  def listButtons = { 
    buttonsTemplate({
      <tr>
           <td/>
      </tr>})
   }
    
  // devo inserire i link sopra e i bottoni sotto
  def _showAllTemplate =
  <lift:crud.all>
     <crud:listMenus/>
     <table id={showAllId} class={showAllClass}>
      <thead>
      <tr>
        <crud:header_item><crud:name/></crud:header_item>
      </tr>
      {if (!fieldsSearchable.isEmpty)
      <tr>
        <crud:header_item><crud:search/></crud:header_item>
      </tr>
      else Nil}
      </thead>
      <tbody>
        <crud:row>
            <crud:row_item><crud:value/></crud:row_item>
        </crud:row>
      </tbody>
    </table>
      <crud:pager>
		<pager:firstP>{pagerFirst}</pager:firstP>
		<pager:prev>{pagerPrev}</pager:prev>
		<pager:actP/>
		<pager:next>{pagerNext}</pager:next>
		<pager:lastP>{pagerLast}</pager:lastP>
	  </crud:pager>
      <crud:listButtons/>
  </lift:crud.all>

  def pagerFirst = S.?("First")
  def pagerPrev = S.?("Prev.")
  def pagerNext = S.?("Next")
  def pagerLast = S.?("Last")

  var orderField: OwnedField[CrudType] = primaryKey
  
  var orderType = "Ascending"
  
  var doSortFields: Function2[CrudType,CrudType,Boolean] =
    (item1: CrudType,item2: CrudType) => { 
      (fieldByName(orderField.name,item1),fieldByName(orderField.name,item2)) match {
        case (Full(e1),Full(e2)) => if (orderType=="Ascending") ((e1.displayName) compareTo (e2.displayName)) < 0
                                    else  ((e1.displayName) compareTo (e2.displayName)) > 0
        case _ => true 
      }
    }
  
// Bisogna aggiungere il modo di fare le query 
  
  def findForList(start: Int, count: Int): List[CrudType] =
    findAll.slice(start,start+count).sort((e1,e2) => doSortFields.apply(e1,e2))

  def me = this // Per poter chiamare i metodi su un CrudType anche da dentro gli scope privati degli snippet
  
  /* I must check this createion method...but it looks working well... */
  var prototype : CrudType = createRecord
  def getEmptyFrom(original: CrudType): CrudType = {
    for (f <- fields)
        fieldByName(f.name, original).open_!.setFromAny(Empty)
    
    original
  }
  
  lazy val locSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "crud.all" => doCrudAll
      case "crud.create" => {
        crudDoForm( getEmptyFrom(prototype) , item => createdMessage(item), item => orderedCreateFields(item))
      }
    }
    def doCrudAll(in: NodeSeq): NodeSeq = {
      val _orderT = S.param("orderType") openOr "desc" // possibili valori "asc" e "desc"
      val orderT = if (_orderT=="desc") "Descending"
                   else "Ascending"
      val ot = if (orderT=="Ascending") "desc"
                   else "asc"
      val _orderF = fields.find{f => f.name==(S.param("orderField") openOr "primarykey")}
      val orderF: OwnedField[CrudType] = if (_orderF.isEmpty) primaryKey
                   else (_orderF) match {
                     case Some(f) => fieldByName(f.name).open_!	// da testare....
                     case _ => primaryKey
                   }
      val _first = S.param("first").map(toInt) openOr 0
      
      // To do
      //val searchFields : scala.collection.mutable.Map[String,String] = 
    	//  scala.collection.mutable.Map()++(searchableItems.elements.map(elem => elem -> (S.get(elem+"-field") openOr "")))
        
      orderType = orderT
      orderField = orderF
      
      val _list = findForList(_first, (rowsPerPage).toInt)
      val list = if (_list.isEmpty) findForList(0, (rowsPerPage).toInt)
                 else _list
      val first: Long = if (_list.isEmpty) 0L
                  else _first
       
      val elements = findAll.size
      
      def sortableLink(f: OwnedField[CrudType]): NodeSeq = {
    	  
          val thClass = if (orderF.name!=f.name) "header"
                        else {
                        	if (S.param("orderType").isEmpty) "header"
                        	else if (ot=="asc") "headerSortUp"
                        	else "headerSortDown"
                        }
          
          <th class={thClass}>
          <a id={f.name+"-link"} href={listPathString+"?first="+first+"&orderType="+ot+"&orderField="+f.name}>
          <div>
    	  {f.displayName}
    	  </div>
          </a>
          </th>
      }
      
      //Finto ma per adesso non lo uso
      def serachableField(f: OwnedField[CrudType]): NodeSeq = {
    	  if (fieldsSearchable.contains(f.name)) {
    	    <th>
    	    {f match {
      				  case x: OwnedField[_] => 
      				  					{((SHtml.ajaxText(f.displayName,value => JsCmds.Noop)))}
      				  }
    	    }
    	  </th>
    	  }
    	  else <th>&nbsp;</th>
      }

      def pager(in: NodeSeq): NodeSeq = {
      
        def prev(in: NodeSeq) = if (first < rowsPerPage) <xml:group>&nbsp;</xml:group>
        else <a href={listPathString+"?first="+(0L max (first - rowsPerPage))+"&orderType="+ot+"&orderField="+orderF.name}>{in}</a>

        def next(in: NodeSeq) = if (list.length < rowsPerPage || elements==(first+rowsPerPage)) <xml:group>&nbsp;</xml:group>
        else <a href={listPathString+"?first="+(first + rowsPerPage)+"&orderType="+ot+"&orderField="+orderF.name}>{in}</a>
      
        def firstPage(in: NodeSeq): NodeSeq = if (elements > rowsPerPage && first!=0) <a href={listPathString+"?first=0"+"&orderType="+ot+"&orderField="+orderF.name}>{in}</a>
                                            else <div/>
        def lastPage(in: NodeSeq): NodeSeq = if (elements > rowsPerPage && first!=((Math.ceil(elements/rowsPerPage)*rowsPerPage).toInt-(subOne_?(elements,rowsPerPage)*rowsPerPage)))
                                                 <a href={listPathString+"?first="+((Math.ceil(elements/rowsPerPage)*rowsPerPage).toInt-(subOne_?(elements,rowsPerPage)*rowsPerPage))+"&orderType="+ot+"&orderField="+orderF.name}>{in}</a>
                                           else <div/>                                     
        def actualPage = if (elements < rowsPerPage) <div/>
                       else <input type="text" class="pagedisplay" disabled="disabled" value={(Math.ceil(first/rowsPerPage)+1).toInt+"/"+(Math.ceil(elements/rowsPerPage)+addOne_?(elements,rowsPerPage)).toInt}/>
      
        def addOne_?(n: Long, d: Long): Int = 
        	if (n%d==0) 0
        	else 1
        def subOne_?(n: Long, d: Long): Int = 
        	if (n%d==0) 1
        	else 0
        
        bind("pager", {if (elements > rowsPerPage) <div id="pager" class="tablesorterPager">{in}</div>
        			  else <div/>},
           "prev" -> prev _, "next" -> next _, "actP" -> actualPage,
      	   "firstP" -> firstPage _, "lastP" -> lastPage _ )
                                                                                  
      }
      
      def doHeaderItems(in: NodeSeq): NodeSeq = 
    	  orderedListFields(me).
    	  flatMap(f => bind("crud", in, "name" -> sortableLink(f), "search" -> serachableField(f)))
         
      def calcLink(f: OwnedField[CrudType]): NodeSeq = 
    	  (fieldsLinkable.find(i => f.name == i._1.name)) match {
    	    case Some(x) => if (x._3) <a href={x._2+f.owner.primaryKey}>{f.toXHtml}</a>
                            else <a href={x._2}>{f.toXHtml}</a>
    	    case _ => f.toXHtml
    	  } 
        
      def doFieldHtml(f: OwnedField[CrudType]): NodeSeq = <td>
      {calcLink(f)}
      <!--	<a href={viewPathString+"/"+f.owner.primaryKey.value}>{f.toXHtml}</a>-->
      </td>
      
      def doRows(in: NodeSeq): NodeSeq =
      list.flatMap{
        c =>
        def doRowItem(in: NodeSeq): NodeSeq = orderedListFields(c).flatMap(
          f => bind("crud", in, "value" -> doFieldHtml(f)))
          
        bind("crud", if (list.indexOf(c)%2==0) <tr class="even">{in}</tr>
    	  			 else <tr class="odd">{in}</tr> ,
             "row_item" -> doRowItem _)
      }

      bind("crud", in, "header_item" -> doHeaderItems _,
           "row" -> doRows _,
           "pager" -> pager _,
      	   "listMenus" -> listUpMenus,
      	   "listButtons" -> listButtons)
    }
  }
   
}
