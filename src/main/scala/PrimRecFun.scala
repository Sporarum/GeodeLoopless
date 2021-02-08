import scala.compiletime.S

sealed trait PrimRecFun[A <: Arity]:
  type V = Vector[Nat, A]
  def apply(args: V): Nat
  def debug(args: V): (Nat, String) = 
    this match
      case UserDefined(_, f) => f.debug_(args) //If we are debugging a user defined function, debug the inside
      case _ => this.debug_(args)
    
  def debug_(args: V): (Nat, String)

  def prettyDebug(args: V): String =
    val string = debug(args)._2
    Util.indentFromBraces(string.split("\n").toList).mkString("\n")


sealed case class UserDefined[A <: Arity](name: String, f: PrimRecFun[A]) extends PrimRecFun[A]:
  def apply(args: V) = f(args)
  def debug_(args: V) = 
    val res = apply(args) //hides the encased complexity for debugging
    (res, s"Function $name on args: $args\nReturned: $res")
  override def toString = name


//f(X) = constant
sealed case class Const[A <: Arity](val constant: Nat) extends PrimRecFun[A]:
  def apply(args: V) = constant
  def debug_(args: V) =
    (apply(args), s"Const($constant) on args: $args") //print arity ?


//f(X) = x_n
sealed case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A]:
  def apply(args: V) = args(n)
  def debug_(args: V) =
    (apply(args), s"Proj($n) on args: $args")


//f(x) = Succ(x)
case object Succ extends PrimRecFun[1]:
  def apply(args: V) = SuccNat(args(0))
  def debug_(args: V) =
    (apply(args), s"Succ on args: $args") 


//f(X) = g(f_1(X), f_2(X), ... , f_A1(X)) with X a vector of arity A2
sealed case class Comp[A1 <: Arity, A2 <: Arity](g: PrimRecFun[A1], fs: Vector[PrimRecFun[A2], A1]) extends PrimRecFun[A2]:
  def apply(args: V) = g(fs.map(_(args)))
  def debug_(args: V) =
    val (resultsFs, strings) = fs.map(_.debug_(args)).unzip
    val stringsWithNames = strings.toList().zipWithIndex.map{case (s, m) => s"f_$m: {\n$s\n}"} //add result after closing brace ?
    val stringFs: String = stringsWithNames.mkString(sep="\n")
    val (resultG, stringG) = g.debug_(resultsFs)
    (resultG, s"Comp on args: $args {\n$stringFs\ng: {\n$stringG\n}: $resultG\n}: $resultG")


// f(X :+ 0) = init(X) with X a vector of arity A1
// f(X :+ S(n)) = step(X :+ n :+ f(X :+ n))
sealed case class Rec[A1 <: Arity](base: PrimRecFun[A1], step: PrimRecFun[S[S[A1]]]) extends PrimRecFun[S[A1]]:
  def apply(args: V) = 
    val last = args.last
    val init: Vector[Nat, A1] = args.init
    last match
      case ZeroNat => base(init)
      case SuccNat(n) =>
        step(init :+ n :+ apply(init :+ n))

  def debug_(args: V) =
    def debug_inner(args: V): (Nat, String) =
      val last = args.last
      val init: Vector[Nat, A1] = args.init
      last match
        case ZeroNat => 
          val (res, s) = base.debug_(init)
          (res, s"Rec calls init: {\n$s\n}: $res")
        case SuccNat(pred) =>
          val (recRes, recS) = debug_inner(init :+ pred)
          val (stepRes, stepS) = step.debug_(init :+ pred :+ recRes)
          (stepRes, recS ++ "\n" ++ f"Rec calls step: {$n$stepS$n}: $stepRes")

    val (res, s) = debug_inner(args)
    (res, s"Rec on args: $args {\n$s\n}: $res")

extension [A0 <: Arity] (g: PrimRecFun[1])
  def apply(f0: PrimRecFun[A0]) =
    g.on(f0)
  def on(f0: PrimRecFun[A0]) =
    Comp[1, A0](g, Vector(f0))

extension [A0 <: Arity] (g: PrimRecFun[2])
  def apply(f0: PrimRecFun[A0], f1: PrimRecFun[A0]) =
    g.on(f0, f1)
  def on(f0: PrimRecFun[A0], f1: PrimRecFun[A0]) =
    Comp[2, A0](g, f0 +: f1 +: VNil)

extension [A0 <: Arity] (g: PrimRecFun[3])
  def apply(f0: PrimRecFun[A0], f1: PrimRecFun[A0], f2: PrimRecFun[A0]) =
    g.on(f0,f1,f2)
  def on(f0: PrimRecFun[A0], f1: PrimRecFun[A0], f2: PrimRecFun[A0]) =
    Comp[3, A0](g, f0 +: f1 +: f2 +: VNil)
