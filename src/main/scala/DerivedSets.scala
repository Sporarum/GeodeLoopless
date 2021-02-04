import scala.language.implicitConversions

def chi_∅[A <: Arity]: PrimRecFun[A] = Const(0)
def empty[A <: Arity] = PrimRecSet(chi_∅[A])
def ∅[A <: Arity] = empty[A]

def chi_full[A <: Arity]: PrimRecFun[A] = Const(1)
def full[A <: Arity] = PrimRecSet(chi_full[A])


def greater: PrimRecFun[2] = Const(1) ∸ (Const(1) ∸ (Proj(1) ∸ Proj(0)))
def greaterSet = PrimRecSet(greater)