package net.liftweb.record

import _root_.net.liftweb.util._//{Box,Full,Empty}
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Helpers.clean

trait KeyedMetaRecord[KeyType,BaseRecord <: KeyedRecord[BaseRecord,KeyType]] extends MetaRecord[BaseRecord] {
  self: BaseRecord =>
  
  def displayClassName = _internalClassName
  def internalClassName = _icName
  
  private lazy val _icName = clean(_internalClassName.toLowerCase)
  private def _internalClassName = getClass.getSuperclass.getName.split("\\.").toList.last
  
  def save(inst:BaseRecord):Boolean
  def saved_?(inst:BaseRecord):Boolean
  def delete_!(inst:BaseRecord):Boolean
  
  def findAll:List[BaseRecord] 
  
  def find(key:KeyType):Box[BaseRecord]
  
  def findAll(by: QueryParam[BaseRecord]*): List[BaseRecord]
  
  def exists(key:KeyType):Boolean
}

class QueryParams[O <: Record[O]](lop: LOp,qp: List[QueryParam[O]],qps: List[QueryParams[O]]) {
  def getQPS = qps
  def getQP = qp
  def getLop = lop
}

trait LOp {
  def xPath: String
}
object And extends LOp {
  def xPath = "and"
}
object Or extends LOp {
  def xPath = "or"
}

sealed trait QueryParam[O <: Record[O]]
case class Cmp[O <: Record[O], T](field:Field[T,O],opr:OprEnum.Value, value:Box[T], otherField:Box[Field[T,O]]) extends QueryParam[O]
case class OrderBy[O <: Record[O], T](field:Field[T,O],
                                      order:AscOrDesc) extends QueryParam[O]

trait AscOrDesc {
  def xPath
}  

case object Ascending extends AscOrDesc {
  def xPath = "ascending"
}

case object Descending extends AscOrDesc {
  def xPath = "descending"
}

object Like {
  def apply[O <: Record[O]](field: Field[String, O], value: String) =
  Cmp[O, String](field, OprEnum.Like, Full(value), Empty)
}

object By {
  import OprEnum._

  def apply[O <: Record[O], T, U <% T](field: Field[T, O], value: U) = Cmp[O,T](field, Eql, Full(value), Empty)
  
  def apply[O <: Record[O],T,  Q <: KeyedRecord [Q,T]](field: ReferenceField[T, O, Q], value: Q) = 
        Cmp[O,T](field, Eql, Full(value.primaryKey.value), Empty)

  def apply[O <: Record[O],T, Q <: KeyedRecord[Q,T]](field: ReferenceField[T, O, Q], value: Box[Q]) =
  value match {
    case Full(v) => Cmp[O,T](field, Eql, Full(v.primaryKey.value), Empty)
    case _ => Cmp(field, null, Empty, Empty) // da verificare
  }
}

object NotBy {
  import OprEnum._

  def apply[O <: Record[O], T, U <% T](field: Field[T, O], value: U) = Cmp[O,T](field, <>, Full(value), Empty)
  def apply[O <: Record[O],T,  Q <: KeyedRecord[Q, T]](field: ReferenceField[T, O, Q], value: Q) =
  Cmp[O,T](field, <>, Full(value.primaryKey.value), Empty)
  def apply[O <: Record[O],T, Q <: KeyedRecord[Q, T]](field: ReferenceField[T, O, Q], value: Box[Q]) =
  value match {
    case Full(v) => Cmp[O,T](field, <>, Full(v.primaryKey.value), Empty)
    case _ => Cmp(field, null, Empty, Empty) // da verificare
  }
}

object ByRef {
  import OprEnum._

  def apply[O <: Record[O], T](field: Field[T, O], otherField: Field[T,O]) = Cmp[O,T](field, Eql, Empty, Full(otherField))
}

object NotByRef {
  import OprEnum._

  def apply[O <: Record[O], T](field: Field[T, O], otherField: Field[T,O]) = Cmp[O,T](field, <>, Empty, Full(otherField))
}

object By_> {
  import OprEnum._

  def apply[O <: Record[O], T, U <% T](field: Field[T, O], value: U) = Cmp[O,T](field, >, Full(value), Empty)
  def apply[O <: Record[O], T](field: Field[T, O], otherField: Field[T,O]) = Cmp[O,T](field, >, Empty, Full(otherField))
}

object By_< {
  import OprEnum._

  def apply[O <: Record[O], T, U <% T](field: Field[T, O], value: U) = Cmp[O,T](field, <, Full(value), Empty)
  def apply[O <: Record[O], T](field: Field[T, O], otherField: Field[T,O]) = Cmp[O,T](field, <, Empty, Full(otherField))
}
  

// Like da correggerre e implementare contains
object OprEnum extends Enumeration {  
  val Eql = Value(1, "=")  
  val <> = Value(2, "!=")  
  val >= = Value(3, ">=")  
  val != = <>  
  val <= = Value(4, "<=")  
  val > = Value(5, ">")  
  val < = Value(6, "<")
  val Like = Value(9, "eq")  
} 