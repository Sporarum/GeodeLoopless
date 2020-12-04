
trait PrimRecFun[A <: Arity] {
  type V = Vector[Nat, A]
  def apply(args: Vector[Nat, A]): Nat
}

//f(X) = c
case class Const[A <: Arity](val constant: Nat) extends PrimRecFun[A] {
  def apply(args: V) = constant
}

//f(X) = x_n
case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A] {
  def apply(args: V) = args(n)
}

//f(x) = Succ(x)
case object Succ extends PrimRecFun[1] {
  def apply(args: V) = SuccNat(args(0))
}

//f(X) = g(f_1(X), f_2(X), ... , f_A1(X)) with X a vector of arity A2
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
