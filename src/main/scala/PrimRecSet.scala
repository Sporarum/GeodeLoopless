


sealed case class PrimRecSet[A <: Arity](chi: PrimRecFun[A]) {
  def contains(elem: Vector[Nat, A]): Boolean = 
    chi(elem) match {
      case ZeroNat => false 
      case _ => true
    }
}