import scala.compiletime.S
import scala.compiletime.ops.int._
import scala.quoted._


trait Vector[T, A <: Arity]{
  def apply[At <: Arity](index: At): T
}
case object VNil                                          extends Vector[Any, 0]{
  def apply[At <: Arity](index: At): Unit = ()
}
case class Cons[T, A <: Arity](head: T, tail: Vector[T,A]) extends Vector[T, S[A]]{

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

  private def dirtyTrick(i: Int): Arity = {
    return i
  }
}