package com.monadsoft.collection
import java.lang.ref.{Reference, SoftReference}

/**
 * //TODO make weakref
 * unordererd softreference collection
 * @author kentomasui
 */
class SoftRefCollection[T] extends RefCollection[T]{
  override def toRef(v: T): Reference[T] = new SoftReference[T](v)
}
