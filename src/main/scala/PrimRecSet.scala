


sealed case class PrimRecSet[A <: Arity](chi: PrimRecFun[A]):
  def contains(elem: Vector[Nat, A]): Boolean = 
    chi(elem) match
      case ZeroNat => false 
      case SuccNat(ZeroNat) => true
      case _ => throw IllegalStateException("This set's characteristic function returned something different from 0 or 1")
    