import scala.language.implicitConversions

def chi_∅[A <: Arity]: PrimRecFun[A] = Const(0)
def empty[A <: Arity] = PrimRecSet(chi_∅[A])
def ∅[A <: Arity] = empty[A]

def chi_full[A <: Arity]: PrimRecFun[A] = Const(1)
def full[A <: Arity] = PrimRecSet(chi_full[A])


def smaller: PrimRecFun[2] = UserDefined("smaller", sign on (Proj(1) ∸ Proj(0)))
def smallerSet = PrimRecSet(smaller)

def areEqual: PrimRecFun[2] = UserDefined("areEqual", not on diff )
def areEqualSet = PrimRecSet(areEqual)

def chi_singleton(n: Nat): PrimRecFun[1] = areEqual(Proj(0), Const(n))
def singleton(n: Nat) = PrimRecSet(chi_singleton(n))

def union[A <: Arity](s0: PrimRecSet[A], s1: PrimRecSet[A]): PrimRecSet[A] = PrimRecSet(sign(s0.chi + s1.chi))

def intersection[A <: Arity](s0: PrimRecSet[A], s1: PrimRecSet[A]): PrimRecSet[A] = PrimRecSet(s0.chi * s1.chi)

def complement[A <: Arity](s0: PrimRecSet[A]) = PrimRecSet(not(s0.chi))

extension[A <: Arity] (s0: PrimRecSet[A]):
    inline def ∪(s1: PrimRecSet[A]) = union(s0,s1)
    inline def ∩(s1: PrimRecSet[A]) = intersection(s0,s1)
    inline def ᶜ: PrimRecSet[A] = complement(s0)

