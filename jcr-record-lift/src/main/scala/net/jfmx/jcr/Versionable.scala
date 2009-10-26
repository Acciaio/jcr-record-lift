package net.jfmx.jcr

trait Versionable {
  self : JCRNodeType[_] =>
  jcrMixinTypes = "Referenceable" :: jcrMixinTypes
}
