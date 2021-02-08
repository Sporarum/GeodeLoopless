import scala.compiletime.S
import scala.language.implicitConversions
import scala.language.postfixOps

def identity[A <: Arity](n: A): PrimRecFun[A] = ???

def subDotOne: PrimRecFun[1] = UserDefined("subDotOne", Rec(Const(0),Proj(0)))

def subDot: PrimRecFun[2] = UserDefined("subDot", Rec(Proj(0), subDotOne on Proj(2)))

def add: PrimRecFun[2] = UserDefined("add", Rec(Proj(0), Succ on Proj(2)))

def mult: PrimRecFun[2] = UserDefined("mult", Rec(Const(0), Proj(2) + Proj(0)))

def exp(base: Nat): PrimRecFun[1] = UserDefined("exp", Rec(Const(0), Proj(1) * Const(base)))

def fact: PrimRecFun[1] = UserDefined("fact", Rec(Const(1), Proj(1) * Succ.on(Proj(0))))

def not: PrimRecFun[1] = UserDefined("not", Const(1) ∸ Proj(0))

def sign: PrimRecFun[1] = UserDefined("sign", not on not on Proj(0))

def diff: PrimRecFun[2] = UserDefined("diff", (Proj[2](0) ∸ Proj(1)) + (Proj[2](1) ∸ Proj(0)))


//A \ B = A ∩ Bᶜ
// (A ∪ B)ᶜ = Aᶜ ∩ Bᶜ
//A \ (B ∪ C) = A ∩ (B ∪ C)ᶜ = A ∩ Bᶜ ∩ Cᶜ 
def caseStudy[A <: Arity, NumSets <: Arity](functions: Vector[PrimRecFun[A], S[NumSets]], sets: Vector[PrimRecSet[A], NumSets]): PrimRecFun[A] = 
    //full, s0ᶜ, s0ᶜ ∩ s1ᶜ, s0ᶜ ∩ s1ᶜ ∩ s2ᶜ, ...
    val restrictors = sets.scanLeft(full[A]){case (s0, s1) => s0 ∩ (s1 ᶜ)}

    //s0, s1 \ s0, s2 \ (s0 ∪ s1), ... , full \ (s0 ∪ s1 ∪ ...)
    val cases = (sets :+ full[A]).zip(restrictors).map{case (set, restrictor) => set ∩ restrictor}

    //x in s0 -> f0(x), x in s1 \ s0 -> f1(s), ...
    functions.zip(cases).map{case (f, c) => f * c.chi}.fold(Const(0)){case (acc, f) => acc + f}

//X :+ y -> sum from t=0 to t=y of f(X :+ t)
def sum[A <: Arity](f: PrimRecFun[A])(using a: A): PrimRecFun[A] =
    def case0 = Const[P[A]](0)
    // X :+ t :+ acc -> f(X :+ t)
    def fTweaked: PrimRecFun[S[A]] = Comp(f, Vector.filled[PrimRecFun[S[A]], A](a)(i => Proj(i))) //TODO: replace when variable substitution added
    // X :+ t :+ acc -> f(X :+ t) + acc
    def caseSn: PrimRecFun[S[A]] = fTweaked + Proj(a)
    // sum(X :+ 0) -> 0
    // sum(X :+ S(t)) -> f(X :+ t) + sum(X :+ t)
    Rec(case0, caseSn)

//f(z *: X) = 0 if for all t <= z: (t *: X) not in A
def boundedMin[A <: Arity](set: PrimRecSet[A]): PrimRecFun[A] = ???




extension[A <: Arity] (f0: PrimRecFun[A]):
    inline def +(f1: PrimRecFun[A]) = add on (f0, f1)
    inline def ∸(f1: PrimRecFun[A]) = subDot on (f0, f1)
    inline def *(f1: PrimRecFun[A]) = mult on (f0, f1)
    inline def ^(n: Nat) = exp(n) on f0
    inline def ! = fact on f0
    //inline def ? = sign on f0 //Keep this ?
    //inline def unary_¬() = not on f0 // not working for some reason