import scala.compiletime.S
import scala.quoted._


trait Vector[T, A <: Arity]{
  def apply[At <: Arity](index: At): T
  def map[U](f: T => U): Vector[U, A]
}

object Vector{
  def apply[T](): Vector[T, 0] = VNil[T]()
  def apply[T](e0: T): Vector[T, 1] = Cons(e0, Vector())
  def apply[T](e0: T, e1: T): Vector[T, 2] = Cons(e0, Vector(e1))
}

case class VNil[T]()                                       extends Vector[T, 0]{
  def apply[At <: Arity](index: At): T = throw new IllegalArgumentException(f"Acces on index ${index} of an empty Vector (This means your index was ${index+1} bigger than the size of your Vector)")
  def map[U](f: T => U) = VNil[U]()
}
case class Cons[T, A <: Arity](head: T, tail: Vector[T,A]) extends Vector[T, S[A]]{

  def apply[At <: Arity](index: At): T = {
    //val arityCheck: At <= A = true
    return if(index == 0){
      head
    }else{
      //val nIndex: At-1 = index - 1
      val nIndex: Arity = index - 1
      //val nIndex = dirtyTrick(index-1)
      tail.apply(nIndex)
    }
  }

  private def dirtyTrick(i: Int): Arity = {
    return i
  }

  def map[U](f: T => U) = Cons(f(head), tail.map(f))
}