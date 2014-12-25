package com.monadsoft.collection

import java.lang.ref.{WeakReference, Reference}

/**
 * @author kentomasui
 */
class WeakRefCollection[T] extends RefCollection[T] {
  override def toRef(v: T): Reference[T] = new WeakReference[T](v)
}
