import scala.compiletime.S
import scala.language.implicitConversions
import scala.language.postfixOps

def identity[A <: Arity](n: A): PrimRecFun[A] = ???

def pred: PrimRecFun[1] = UserDefined("pred", Rec(0, Proj(0)))

def subDot: PrimRecFun[2] = UserDefined("subDot", Rec(Proj(0), pred on Proj(2)))

def add: PrimRecFun[2] = UserDefined("add", Rec(Proj(0), Succ on Proj(2)))

def mult: PrimRecFun[2] = UserDefined("mult", Rec(0, Proj(2) + Proj(0)))

def exp(base: Nat): PrimRecFun[1] = UserDefined("exp", Rec(0, Proj(1) * base))

def fact: PrimRecFun[1] = UserDefined("fact", Rec(1, Proj(1) * Succ.on(Proj(0))))

def not: PrimRecFun[1] = UserDefined("not", 1 ∸ Proj(0))

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
    val res: PrimRecFun[A] = functions.zip(cases).map{case (f, c) => f * c.chi}.fold(Const(0)){case (acc, f) => acc + f}
    UserDefined(s"caseStudy($functions, $sets)", res)

def min: PrimRecFun[2] = UserDefined("min", 
    caseStudy(
        Proj[2](0) +: Proj[2](1) +: VNil, 
        smallerSet +: VNil
    ))
def max: PrimRecFun[2] = UserDefined("max", 
    caseStudy(
        Proj[2](1) +: Proj[2](0) +: VNil, 
        smallerSet +: VNil
    ))
//X :+ y -> combination from t=0 to t<y of leaf(X :+ t), with z as zero element => combine(leaf(X :+ (y-1)), ... combine(leaf(X :+ 1), combine(leaf(X :+ 0), z))...)
def exclusiveFold[A <: Arity](zero: PrimRecFun[P[A]], combine: PrimRecFun[2], leaf: PrimRecFun[A])(using a: A): PrimRecFun[A] =
    // X :+ t :+ acc -> leaf(X :+ t)
    def leafTweaked: PrimRecFun[S[A]] = Comp(leaf, Vector.filled[PrimRecFun[S[A]], A](a)(i => Proj(i))) //TODO: replace when variable substitution added
    // X :+ t :+ acc -> combine(leaf(X :+ t), acc)
    def caseSn: PrimRecFun[S[A]] = combine(leafTweaked, Proj(a))
    // fold(X :+ 0) -> z(X)
    // fold(X :+ S(t)) -> combine(leaf(X :+ t), fold(X :+ t))
    val res: PrimRecFun[A] = Rec(zero, caseSn)
    UserDefined(s"exclusiveFold($zero, $combine, $leaf)", res)

//X :+ y -> combination from t=0 to t=y of leaf(X :+ t), with zero as zero element => combine(leaf(X :+ y), ... combine(leaf(X :+ 1), combine(leaf(X :+ 0), zero))...)
def fold[A <: Arity](zero: PrimRecFun[P[A]], combine: PrimRecFun[2], leaf: PrimRecFun[A])(using a: A): PrimRecFun[A] =
    //increments y by 1
    def inc(i: Arity): PrimRecFun[A] = if i == a-1 then Succ(Proj[A](i)) else Proj(i)
    val res: PrimRecFun[A] = Comp(exclusiveFold(zero, combine, leaf), Vector.filled[PrimRecFun[A], A](a)(inc))
    UserDefined(s"fold($zero, $combine, $leaf)", res)

//X :+ y -> sum from t=0 to t<y of f(X :+ t)
def sum[A <: Arity](f: PrimRecFun[A])(using a: A): PrimRecFun[A] =
    fold(zero = Const[P[A]](0), combine = add, leaf = f)

//X :+ y -> product from t=0 to t<y of f(X :+ t)
def product[A <: Arity](f: PrimRecFun[A])(using a: A): PrimRecFun[A] =
    fold(zero = Const[P[A]](1), combine = mult, leaf = f)

def minNotZero: PrimRecFun[2] = 
    val m = Proj[2](0)
    val n = Proj[2](1)
    val res = caseStudy(
        m                   +: n                    +: min +: VNil,
        (n ?= 0)  +: (m ?= 0)   +: VNil
    )
    UserDefined("minNotZero", res)

//f(X :+ z) = 0 if for all t <= z: (X :+ t) not in A
//f(X :+ z) = S(t) for minimal t <= z s.t. (X :+ t) in A
def boundedMinPlusOne[A <: Arity](set: PrimRecSet[A])(using a: A): PrimRecFun[A] =
    val t: PrimRecFun[A] = Proj(minusOne(a))
    val checkAndReturn: PrimRecFun[A] = Succ(t) * set.chi
    fold(zero = 0, combine = minNotZero, leaf = checkAndReturn)

//f(X :+ z) = 0 if for all t <= z: (X :+ t) not in A
//f(X :+ z) = t for minimal t <= z s.t. (X :+ t) in A
def boundedMin[A <: Arity](set: PrimRecSet[A])(using a: A): PrimRecFun[A] = pred on boundedMinPlusOne(set)


extension[A <: Arity] (f0: PrimRecFun[A])
    inline def +(f1: PrimRecFun[A]) = add on (f0, f1)
    inline def ∸(f1: PrimRecFun[A]) = subDot on (f0, f1)
    inline def *(f1: PrimRecFun[A]) = mult on (f0, f1)
    inline def ^(n: Nat) = exp(n) on f0
    inline def ! = fact on f0
    //inline def ? = sign on f0 //Keep this ?
    //inline def unary_¬() = not on f0 // not working for some reason