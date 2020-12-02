import scala.compiletime._
import scala.compiletime.ops.int._
import scala.quoted._




type Arity = Int & Singleton

trait Vector[T, A <: Arity]{
  def apply[At <: Arity](index: At): T
}
case object VNil                                          extends Vector[Any, 0]{
  def apply[At <: Arity](index: At): Unit = Unit
}
case class Con[T, A <: Arity](head: T, tail: Vector[T,A]) extends Vector[T, S[A]]{

  def apply[At <: Arity](index: At): T = {
    //val arityCheck: At <= A = true
    return if(index == 0){
      head
    }else{
      //val nIndex: At-1 = index - 1
      val nIndex = dirtyTrick(index-1)
      tail.apply(nIndex)
    }
  }
}

def dirtyTrick(i: Int): Arity = {
  return i
}