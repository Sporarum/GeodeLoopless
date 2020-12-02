import scala.compiletime._
import scala.compiletime.ops.int._

/*
sealed trait VectorOld[T, A <: Arity]{
  def apply[At <: Arity](index: At): T
}

object VectorOld {

  type Head[T, A <: Arity, X <: VectorOld[T, A]] = X match { //T+ ? T- ?
    case VectorOldCons[t, a, v] => t
  }

  type Tail[T, A <: Arity, X <: VectorOld[T, S[A]]] <: VectorOld[T, A] = X match {
    case VectorOldCons[t, a, v] => v
  }

  type Concat[T, A1 <: Arity, X <: VectorOld[T, A1], A2 <: Arity, Y <: VectorOld[T, A2]] <: VectorOld[T, A1 + A2] = X match {
    case VectorOld[T, 0] => Y
    case VectorOldCons[T, a1, t1] => //
      Y match {
        case VectorOld[T, 0] => VectorOldCons[T, a1 + 0, t1]
        case VectorOld[T, a2] => VectorOldCons[T, a1 + a2 , Concat[T, a1 + a2 , t1, A2, Y]]
      } 
  }

  def apply(): VectorOld[Any, 0] = VNil

  def apply[T](x: T): VectorOld[T, 1] = ???

  def unapply(x: VectorOld[_, 0]): true = true

}

object VOldNil extends VectorOld[Any, 0] {
  def apply[At <: Arity](index: At) = Unit
}
//A is the arity of the tail VectorOld, not of the resulting VectorOld !
sealed abstract class VectorOldCons[T, A <: Arity, V <: VectorOld[T, A]] extends VectorOld[T, S[A]]




object VectorOldCons {
  def apply[H, A <: Arity, T <: VectorOld[H, A]](h: H, t: T): VectorOldCons[H, A, T] = ???
  def unapply[H, A <: Arity, T <: VectorOld[H, A]](x: VectorOldCons[H, A, T]): (H, T) = ???
}

*/