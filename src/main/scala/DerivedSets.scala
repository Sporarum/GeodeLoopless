import scala.compiletime.ops.int.+
import scala.language.implicitConversions
import scala.language.postfixOps

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

def complement[A <: Arity](s0: PrimRecSet[A]): PrimRecSet[A] = PrimRecSet(not(s0.chi))

def nDsFrom1Ds[A <: Arity](oneDs: Vector[PrimRecSet[1], A]): PrimRecSet[A] = 
    //chis of sets where the i'th parameter is in oneDs(i)
    val projs: Vector[PrimRecFun[A], A] = oneDs.zipWithIndex.map{case (PrimRecSet(chi), i) => chi on Proj[A](i)}
    //the chi of the intersection of previous chis => set where for all i: the i'th parameter is in oneDs(i)
    val chi_intersection: PrimRecFun[A] = projs.fold(Const(1)){case (f0, f1) => f0 * f1}
    PrimRecSet(chi_intersection)

def nDSingleton[A <: Arity](nats: Vector[Nat, A]): PrimRecSet[A] = nDsFrom1Ds(nats.map(singleton(_)))

def boundedExists[A <: Arity](set: PrimRecSet[A])(using a: A): PrimRecSet[A] =
    PrimRecSet(sign on sum(set.chi))

def boundedForAll[A <: Arity](set: PrimRecSet[A])(using a: A): PrimRecSet[A] =
    PrimRecSet(product(set.chi))

def cartesianProduct[A0 <: Arity, A1 <: Arity](s0: PrimRecSet[A0], s1: PrimRecSet[A1])(using a0: A0, a1: A1): PrimRecSet[A0 + A1] =
    val p0 = Vector.filled(a0)(i => Proj[A0 + A1](i))
    val ns0 = p0 in s0

    val p1 = Vector.filled(a1)(i => Proj[A0 + A1](a0 + i))
    val ns1 = p1 in s1
    
    ns0 ∩ ns1

extension[A <: Arity, B <: Arity] (s0: PrimRecSet[A])
    inline def ∪(s1: PrimRecSet[A]) = union(s0,s1)
    inline def ∩(s1: PrimRecSet[A]) = intersection(s0,s1)
    inline def ᶜ: PrimRecSet[A] = complement(s0)
    inline def x(s1: PrimRecSet[B])(using A, B): PrimRecSet[A + B] = cartesianProduct(s0, s1)

extension[A <: Arity] (f0: PrimRecFun[A])
    inline def <(f1: PrimRecFun[A]) = PrimRecSet(smaller on (f0, f1))
    inline def >(f1: PrimRecFun[A]) = PrimRecSet(smaller on (f1, f0))
    inline def <=(f1: PrimRecFun[A]) = ((f0 > f1)ᶜ)
    inline def >=(f1: PrimRecFun[A]) = ((f0 < f1)ᶜ)
    inline def ?=(f1: PrimRecFun[A]) = PrimRecSet(areEqual on (f0, f1))
    inline def !=(f1: PrimRecFun[A]) = ((f0 ?= f1)ᶜ)

extension[A <: Arity, OtherA <: Arity](v0: Vector[PrimRecFun[OtherA], A])
    inline def in(s0: PrimRecSet[A]): PrimRecSet[OtherA] = PrimRecSet(Comp(s0.chi, v0))
