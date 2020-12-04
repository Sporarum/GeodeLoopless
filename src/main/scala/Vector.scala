import scala.compiletime.S
import scala.compiletime.error
import annotation.showAsInfix
import scala.annotation.implicitNotFound
import scala.quoted._

trait Vector[T, A <: Arity]{
  /*@implicitNotFound("index out of bounds")
  type Check[At <: Arity] <: T = A <= At match {
    case true => T
    //case _ => error("")
  }*/
  def apply[At <: Arity](index: At): T
  def map[U](f: T => U): Vector[U, A]
  def head(): T
  def tail(): Vector[T, P[A]]
  inline def *: (x: T): Vector[T, S[A]] = Cons(x, this)
}

object Vector{
  def apply[T](): Vector[T, 0] = VNil[T]()
  def apply[T](e0: T): Vector[T, 1] = Cons(e0, Vector())
  def apply[T](e0: T, e1: T): Vector[T, 2] = Cons(e0, Vector(e1))
}

case class VNil[T]() extends Vector[T, 0]{
  def apply[At <: Arity](index: At): T = throw new IllegalArgumentException(f"Acces on index ${index} of an empty Vector (This means your index was ${index+1} bigger than the size of your Vector)")
  def map[U](f: T => U) = VNil[U]()
  def head() = throw new IllegalArgumentException(f"Empty Vector has no head")
  def tail() = throw new IllegalArgumentException(f"Empty Vector has no tail")
}

case class Cons[T, A <: Arity](h: T, t: Vector[T,A]) extends Vector[T, S[A]]{
  def apply[At <: Arity](index: At): T = {
    return if(index == 0){
      h
    }else{
      val nIndex: P[At] = minusOne(index);
      t.apply(nIndex)
    }
  }

  def map[U](f: T => U) = Cons(f(h), t.map(f))
  def head() = h
  def tail() = t
}