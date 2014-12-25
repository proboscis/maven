package com.monadsoft.collection

import java.lang.ref.Reference

import scala.collection.mutable

/**
 * //TODO make weakref
 * unordererd softreference collection
 * @author kentomasui
 */
abstract class RefCollection[T] extends Traversable[T] with mutable.Buffer[T]{
  import com.badlogic.gdx.utils.{Array => GdxArray}
  val refs = new GdxArray[Reference[T]](false,20)
  def toRef(v:T):Reference[T]
  override def foreach[U](f: (T) => U): Unit = {
    val it = refs.iterator()
    while(it.hasNext){
      val next = it.next()
      val ref = next.get()
      if(ref == null){
        it.remove()
        //println("removed a gc processed ref")
      } else{
        f(ref)
      }
    }
  }
  override def apply(n: Int): T = refs.get(n).get()
  override def update(n: Int, newelem: T): Unit = refs.set(n,toRef(newelem))
  override def clear(): Unit = refs.clear()
  override def length: Int = refs.size
  override def remove(n: Int): T = refs.removeIndex(n).get()


  override def +=(elem: T) = {
    refs.add(toRef(elem))
    this
  }

  override def +=:(elem: T) = this += elem

  override def insertAll(n: Int, elems: Traversable[T]): Unit = elems.foreach(this.+=)
  override def iterator: Iterator[T] = new Iterator[T]{
    val itr = refs.iterator()
    var curr = next()
    override def hasNext: Boolean = curr != null
    override def next(): T = {
      var res:T = null.asInstanceOf[T]
      while(itr.hasNext && res == null){
        res = itr.next().get()
      }
      res
    }
  }
}
