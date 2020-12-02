
trait PrimRecFun[A <: Arity] {
  def apply(args: Vector[Nat, A]): Nat
}

case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A] {
  def apply(args: Vector[Nat, A]) = args(n)
}

case class Succ() extends PrimRecFun[1] {
  def apply(args: Vector[Nat, 1]) = SuccNat(args(0))
}

case class PrimRecSet[A <: Arity](fun: PrimRecFun[A]) {
  def contains(elem: Vector[Nat, A]): Boolean = 
    fun(elem) match {
      case ZeroNat => false
      case _ => true
    }
}
