
trait PrimRecFun[A <: Arity] {
  def apply(args: Vector[A, Nat]): Nat
}

case class Proj[A](val n: Arity) extends PrimRecFun[A] {
  def apply(args: Vector[A, Nat]) = args(n)
}

case class Succ() extends PrimRecFun[1] {
  def apply(args: Vector[1, Nat]) = SuccNat(args(0))
}

case class PrimRecSet[A <: Arity](fun: PrimRecFun[A]) {
  def contains(elem: Vector[A, Nat]): Boolean = 
    fun(elem) match {
      case ZeroNat => false
      case _ => true
    }
}
