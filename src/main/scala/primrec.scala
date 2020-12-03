
trait PrimRecFun[A <: Arity] {
  type V = Vector[Nat, A]
  def apply(args: Vector[Nat, A]): Nat
}


case class Const[A <: Arity](val constant: Nat) extends PrimRecFun[A] {
  def apply(args: V) = constant
}

case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A] {
  def apply(args: V) = args(n)
}

case class Succ() extends PrimRecFun[1] {
  def apply(args: V) = SuccNat(args(0))
}

case class Comp[A1 <: Arity, A2 <: Arity](g: PrimRecFun[A1], fs: Vector[PrimRecFun[A2], A1]) extends PrimRecFun[A2]{
  def apply(args: V) = g(fs.map(_(args)))
}

case class PrimRecSet[A <: Arity](fun: PrimRecFun[A]) {
  def contains(elem: Vector[Nat, A]): Boolean = 
    fun(elem) match {
      case ZeroNat => false
      case _ => true
    }
}
