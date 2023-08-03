# Geode Loopless
## Type Enforced Primitive Recursive Functions (and Sets)

### Description

Geode Loopless (GeodeL) is a Domain Specific Language that aims at facilitating the writing and analysis of Primitive Recursive Functions and Sets.

This DSL is embedded in Scala, and aims to use its strong type system to enforce the arity and type of the `PrimRec` functions and sets, thus ensuring any function or set is really primitive recursive.

Primitive recursive functions are all the functions on naturals that can be created by combining constants, projections, and the successor function through recursion and composition.

In Scala, this translates to the following ([the actual implementation](src/main/scala/PrimRecFun.scala) also includes a "stack trace" method `debug`):
```scala
import scala.compiletime.S

sealed trait PrimRecFun[A <: Arity]:
  type V = Vector[Nat, A] // sequence of Nats of length A
  val arity: A = valueOf[A]
  def apply(args: V): Nat

//f(X) = constant
sealed case class Const[A <: Arity](val constant: Nat) extends PrimRecFun[A]:
  def apply(args: V) = constant

//f(X) = x_n
sealed case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A]:
  def apply(args: V) = args(n)

//f(x) = Succ(x)
case object Succ extends PrimRecFun[1]:
  def apply(args: V) = SuccNat(args(0))

//f(X) = g(f_1(X), f_2(X), ... , f_ArityG(X)) with X a vector of arity ArityFs
sealed case class Comp[ArityG <: Arity, ArityFs <: Arity](g: PrimRecFun[ArityG], fs: Vector[PrimRecFun[ArityFs], ArityG]) extends PrimRecFun[ArityFs]:
  def apply(args: V) = g(fs.map(f => f(args)))

// f(X :+ 0) = init(X) with X a vector of arity A-1
// f(X :+ S(n)) = step(X :+ n :+ f(X :+ n))
sealed case class Rec[A <: Arity](base: PrimRecFun[P[A]], step: PrimRecFun[S[A]]) extends PrimRecFun[A]:
  def apply(args: V) = 
    val args_ = args.toSPA()
    val last = args_.last
    val init: Vector[Nat, P[A]] = args_.init
    last match
      case ZeroNat => base(init)
      case SuccNat(pred) =>
        val nArgs = (init :+ pred).toA()
        step(nArgs :+ apply(nArgs))
```

You'll notice each `apply` method terminates on all inputs, therefore, assuming there are no other constructors for `PrimRecFun[A]`, for any instance `fun`, `fun.apply` terminates on all inputs!

We encode in [`PrimRecSet.scala`](src/main/scala/PrimRecSet.scala) sets as functions which return either `1` or `0`. This construction gives us sets for which set-membership always terminates!

While our set of building blocks seems constraining at first, it is extremely expressive, even allowing a form of pattern matching:
```scala
// X match
//   case _ if X in s0 => f0(X)
//   case _ if X in s1 => f1(X) // Should only match if X is not in s0, hence the s1 / s0 below (which is equivalent to s1 ∩ s0ᶜ)
//   ...
//   case _            => fn+1(X) // where n is NumSets
def caseStudy[A <: Arity, NumSets <: Arity](functions: Vector[PrimRecFun[A], S[NumSets]], sets: Vector[PrimRecSet[A], NumSets]): PrimRecFun[A] = 
    //full, s0ᶜ, s0ᶜ ∩ s1ᶜ, s0ᶜ ∩ s1ᶜ ∩ s2ᶜ, ...
    val restrictors = sets.scanLeft(full[A]){case (s0, s1) => s0 ∩ (s1 ᶜ)}

    //s0, s1 \ s0, s2 \ (s0 ∪ s1), ... , full \ (s0 ∪ s1 ∪ ...)
    val cases = (sets :+ full[A]).zip(restrictors).map{case (set, restrictor) => set ∩ restrictor}

    //x in s0 -> f0(x), x in s1 \ s0 -> f1(s), ...
    val res: PrimRecFun[A] = functions.zip(cases).map{case (f, c) => f * c.chi}.fold(Const(0)){case (acc, f) => acc + f}
    UserDefined(s"caseStudy($functions, $sets)", res)
```
The above also highlights some DSL features, such as `+`, `*`, `∩` and `ᶜ`.
The goal is to expand these to include, among other things, a nicer syntax for `Rec` and `caseStudy`.

All examples can be found in [`DerivedFunctions.scala`](https://github.com/Sporarum/GeodeLoopless/blob/DerivedFunctions/src/main/scala/DerivedFunctions.scala) and [`DerivedSets.scala`](https://github.com/Sporarum/GeodeLoopless/blob/DerivedFunctions/src/main/scala/DerivedSets.scala) on branch [DerivedFunctions](https://github.com/Sporarum/GeodeLoopless/tree/DerivedFunctions).

Through encoding, it is even possible to have primitive recursive functions on things other than natural numbers. This is one of the next big steps planned for this project.
