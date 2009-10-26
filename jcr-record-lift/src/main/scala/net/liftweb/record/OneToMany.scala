package net.liftweb.record

import _root_.net.liftweb.util._//{Box,Empty,Full}
import _root_.net.liftweb.common._

class OneToMany[K1, K2, Owner <: KeyedRecord[Owner,K2], Other <: KeyedRecord[Other,K1]](field:ReferenceField[K1,Owner,Other])
                        extends MultiReference[K1,Owner,Other](field) { 
  
  def inverse:Box[ManyToOne[K2,K1,Other,Owner]] = Empty
  
  override def add(other:Other) {
    addKey(other.primaryKey.value)
    inverse.foreach{field => 
      foreign.fieldByName(field.name,other).foreach{f => 
        (f.asInstanceOf[ManyToOne[K2,K1,Other,Owner]]).setKey(owner.primaryKey.value)
       
      }   
    }
  } 
  
  override def remove(other:Other) {
    removeKey(other.primaryKey.value)
    inverse.foreach{field => 
      foreign.fieldByName(field.name,other).foreach{
        f => (f.asInstanceOf[ManyToOne[K2,K1,Other,Owner]]).resetKey
      }  
    }  
  }
  
}

                        
trait ManyToOne[K1,K2,Owner <: KeyedRecord[Owner,K2], Other <: KeyedRecord[Other,K1]] extends ReferenceField[K1,Owner,Other] {
  
  def inverse:Box[OneToMany[K2,K1,Other,Owner]] = Empty
  
  override def reset {
    obj.foreach{other => 
      inverse.foreach{field =>
        otherMeta.fieldByName(field.name,other).foreach{f => 
          f.asInstanceOf[OneToMany[K2,K1,Other,Owner]].removeKey(owner.primaryKey.value)
        }
      }
    }
    set(defaultValue)
  }
  
  override def set(other:Other) { 
    obj.foreach{other =>
      inverse.foreach{field =>
        otherMeta.fieldByName(field.name,other).foreach{f =>
          f.asInstanceOf[OneToMany[K2,K1,Other,Owner]].removeKey(owner.primaryKey.value)
        } 
      }  
    }
    inverse.foreach{field => 
      otherMeta.fieldByName(field.name,other).foreach{f =>
        f.asInstanceOf[OneToMany[K2,K1,Other,Owner]].addKey(owner.primaryKey.value)
      }
    }
    set(other.primaryKey.value)
  }
}

trait OneToOne[K1,K2,Owner <: KeyedRecord[Owner,K2], Other <: KeyedRecord[Other,K1]] extends ReferenceField[K1,Owner,Other] {
  
  def inverse:Box[OneToOne[K2,K1,Other,Owner]] = Empty
  
  override def reset {
    obj.foreach{other => 
      inverse.foreach{field =>
        otherMeta.fieldByName(field.name,other).foreach{f => 
          f.asInstanceOf[OneToOne[K2,K1,Other,Owner]].resetKey
        }
      }
    }
    set(defaultValue)
  }
  
  override def set(other:Other) { 
    obj.foreach{other =>
      inverse.foreach{field =>
        otherMeta.fieldByName(field.name,other).foreach{f =>
          f.asInstanceOf[OneToOne[K2,K1,Other,Owner]].resetKey
        } 
      }  
    }
    inverse.foreach{field => 
      otherMeta.fieldByName(field.name,other).foreach{f =>
        f.asInstanceOf[OneToOne[K2,K1,Other,Owner]].setKey(owner.primaryKey.value)
      }
    }
    set(other.primaryKey.value)
  }
}

class ManyToMany[K1,K2,Owner <: KeyedRecord[Owner,K2], Other <: KeyedRecord[Other,K1]](field:ReferenceField[K1,Owner,Other])
extends MultiReference(field) {
  
  def inverse:Box[ManyToMany[K2,K1,Other,Owner]] = Empty
  
  override def add(other:Other) {
    addKey(other.primaryKey.value)
    inverse.foreach{field => 
      foreign.fieldByName(field.name,other).foreach{f => 
        (f.asInstanceOf[ManyToMany[K2,K1,Other,Owner]]).addKey(owner.primaryKey.value)
      }   
    }
  } 
  
  override def remove(other:Other) {
    removeKey(other.primaryKey.value)
    inverse.foreach{field => 
      foreign.fieldByName(field.name,other).foreach{
        f => (f.asInstanceOf[ManyToMany[K2,K1,Other,Owner]]).removeKey(owner.primaryKey.value)
      }  
    }  
  }
  
}



