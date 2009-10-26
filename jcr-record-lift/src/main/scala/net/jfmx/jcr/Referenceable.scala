package net.jfmx.jcr

trait Referenceable { 
  self : JCRNodeType[_] =>
  jcrMixinTypes = "Referenceable" :: jcrMixinTypes
  
}
