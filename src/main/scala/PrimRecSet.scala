


sealed case class PrimRecSet[A <: Arity](chi: PrimRecFun[A]):
  def contains(elem: Vector[Nat, A]): Boolean = 
    chi(elem) match
      case ZeroNat => false 
      case SuccNat(ZeroNat) => true
      case _ => throw IllegalStateException("This set's characteristic function returned something different from 0 or 1")

  def showExamples(nForEachDim: Int)(using a: A): String =
    def update(v: Vector[Nat, A]): Option[Vector[Nat, A]] = 
      def incrementCells(pair: (Nat, Boolean), n: Nat): (Nat, Boolean) =
        val (oldN, done) = pair
        if done then
          (n, true)
        else
          if n.int < nForEachDim then
            (SuccNat(n), true)
          else
            (ZeroNat, false)
      val z = (ZeroNat, false)
      val incremented = v.scanLeft(z)(incrementCells)
      val done = incremented.last._2 
      if !done then
        None
      else
        Some(incremented.tail.map(_._1))
    def rec(v: Vector[Nat, A], acc: List[Vector[Nat, A]]): List[Vector[Nat, A]] =
      val nAcc = if this.contains(v) then v :: acc else acc
      update(v) match
        case Some(nV) => rec(nV, nAcc)
        case None => nAcc
    rec(Vector.filled[Nat, A](a, ZeroNat), Nil).mkString("\n")



      