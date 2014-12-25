package com.monadsoft.reactive

import com.monadsoft.collection.WeakRefCollection

/**
 * @author kentomasui
 * // you gotta keep the ref!
 */
trait Varying[+T] extends Observable[T]{
  protected[reactive] val children = new WeakRefCollection[Varying[_]]
  protected[reactive] def addChild[R](t:Varying[R]): Unit ={
    children += t
  }
  protected[reactive] def invalidate(): Unit ={
    if(children.nonEmpty){
      children.foreach(_.invalidate())
    }
  }
  def map[R](f:T=>R):Varying[R]
  def flatMap[R](f:T=>Varying[R]):Varying[R]
  def sample:T
  def dirty:Boolean
}
trait Observer{
  var observerRefs = List[AnyRef]()
  def observe[T](tgt:Observable[T])(proc:T=>Any):AnyRef={
    val ref = tgt.subscribe(proc)
    observerRefs ::= ref
    ref
  }
}
trait Observable[+T]{
  def subscribe(f:T=>Any):AnyRef
  def unsubscribe(f:T=>Any) = ???
}
trait ObservableImpl[T] extends Observable[T] with Varying[T]{
  val subscribers = new WeakRefCollection[T=>Any]()
  override def subscribe(f: (T) => Any): AnyRef ={
    subscribers += f
    f
  }
  def publish(): Unit ={
    if(subscribers.nonEmpty) {
      subscribers.foreach(_.apply(sample))
    }
  }
}
trait VaryingImpl[T] extends Varying[T]
  with Observer with ObservableImpl[T]{
  override def invalidate(): Unit = {
    super.invalidate()
    publish()
  }
}
class Var[T](var value:T) extends Varying[T] with VaryingImpl[T]{
  self=>
  override def map[R](f: (T) => R): Varying[R] = new Mapped(this,f)
  override def flatMap[R](f: (T) => Varying[R]): Varying[R] = new FlatMapped(this,f)
  override def sample: T = value
  def update(nv:T): Unit ={
    value = nv
    invalidate()
  }
  override def dirty: Boolean = false
}
class Mapped[S,T](src:Varying[S],f:S=>T) extends VaryingImpl[T]{
  self =>
  var dirty = true
  var current:T = null.asInstanceOf[T]
  src.addChild(this)
  override def invalidate(): Unit = {
    dirty = true
    super.invalidate()
  }


  override def map[R](f: (T) => R): Varying[R] = new Mapped(this,f)

  override def flatMap[R](f: (T) => Varying[R]): Varying[R] = new FlatMapped(this,f)

  override def sample: T = if(src.dirty || dirty){
    current = f(src.sample)
    dirty = false
    current
  } else {
    current
  }
}

class FlatMapped[S,T](src:Varying[S],f:S=>Varying[T]) extends VaryingImpl[T]{
  self =>
  var vary = null.asInstanceOf[Varying[T]]
  var dirty = true
  var current:T = null.asInstanceOf[T]
  src.addChild(this)
  override def invalidate(): Unit = {
    dirty = true
    super.invalidate()
  }
  override def map[R](f: (T) => R): Varying[R] = new Mapped(this,f)
  override def flatMap[R](f: (T) => Varying[R]): Varying[R] = new FlatMapped(this,f)
  override def sample: T = {
    if(src.dirty || dirty){
      vary = f(src.sample)
      vary.addChild(this)
      dirty = false
    }
    if(vary.dirty){
      current = vary.sample
    }
    current
  }
}

object Var{
  def apply[T](value:T):Var[T] = new Var(value)
}

object VarTest{
  def main(args: Array[String]) {
    //well, its ok for now..
  }
}


