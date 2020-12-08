import scala.compiletime.S

trait PrimRecFun[A <: Arity] {
  type V = Vector[Nat, A]
  def apply(args: V): Nat
  def debug(args: V): (Nat, String)
  def prettyDebug(args: V): String =
    def indent(s: String, ammount: Int) = tab * ammount ++ s //++ f" t:$ammount"
    def indentFromBraces(s: List[String], depth: Int = 0): List[String] =
      s match {
        case h :: tail if h endsWith "{" =>
          indent(h, depth) :: indentFromBraces(tail, depth+1)
        case h :: tail if h startsWith "}"=>
          if (depth == 0)
            throw new IllegalStateException("unmatched closing brace")
          else
            indent(h, depth-1) :: indentFromBraces(tail, depth-1)
        case h :: tail => 
          indent(h, depth) :: indentFromBraces(tail, depth)
        case Nil =>
          if (depth != 0)
            List(f"!!!$depth unmatched oppening brace${if(depth > 1) "s" else ""}!!!")
          else
            Nil
      }
    val string = debug(args)._2
    indentFromBraces(string.split("\n").toList).mkString("\n")
  private val tab = "  "
}

case class UserDefined[A <: Arity](name: String, f: PrimRecFun[A]) extends PrimRecFun[A] {
  def apply(args: V) = f(args)
  def debug(args: V) = 
    val res = f(args)
    val n = "\n"
    (res, f"Function $name on args: $args${n}Returned: $res")
}

//f(X) = constant
case class Const[A <: Arity](val constant: Nat) extends PrimRecFun[A] {
  def apply(args: V) = constant
  def debug(args: V) =
    (apply(args), f"Const($constant) on args: $args") //print arity ?
}

//f(X) = x_n
case class Proj[A <: Arity](val n: Arity) extends PrimRecFun[A] {
  def apply(args: V) = args(n)
  def debug(args: V) =
    (apply(args), f"Proj($n) on args: $args")
}

//f(x) = Succ(x)
case object Succ extends PrimRecFun[1] {
  def apply(args: V) = SuccNat(args(0))
  def debug(args: V) =
    (apply(args), f"Succ on args: $args") 
}

//f(X) = g(f_1(X), f_2(X), ... , f_A1(X)) with X a vector of arity A2
case class Comp[A1 <: Arity, A2 <: Arity](g: PrimRecFun[A1], fs: Vector[PrimRecFun[A2], A1]) extends PrimRecFun[A2]{
  def apply(args: V) = g(fs.map(_(args)))
  def debug(args: V) =
    val n = '\n'
    val start = f"Comp on args: $args {$n"
    val (resultsFs, strings) = fs.map(_.debug(args)).unzip
    val stringsWithNames = strings.toList().zipWithIndex.map{case (s, m) => f"f_$m: {$n$s$n}"} //add result after } ?
    val stringFs: String = stringsWithNames.mkString(start="", sep="\n", end=f"$n")
    val (resultG, stringG) = g.debug(resultsFs)
    (resultG, start ++ stringFs ++ f"g: {$n$stringG$n}: $resultG" ++ f"$n}: $resultG")

}

// f(0 *: X) = init(X) with X a vector of arity A1
// f(S(n) *: X) = step(f(n *: X) *: n *: X)
case class Rec[A1 <: Arity](init: PrimRecFun[A1], step: PrimRecFun[S[S[A1]]]) extends PrimRecFun[S[A1]]{
  def apply(args: V) = 
    val head = args.head()
    val tail: Vector[Nat, A1] = args.tail()
    head match
      case ZeroNat => init(tail)
      case SuccNat(n) =>
        step(this.apply(n *: tail) *: n *: tail )

  def debug(args: V) =
    val n = '\n'
    def debug_(args: V): (Nat, String) =
      val head = args.head()
      val tail: Vector[Nat, A1] = args.tail()
      head match
        case ZeroNat => 
          val (res, s) = init.debug(tail)
          (res, f"Rec calls init: {$n$s$n}: $res")
        case SuccNat(pred) =>
          val (recRes, recS) = debug_(pred *: tail)
          val (stepRes, stepS) = step.debug(recRes *: pred *: tail)
          (stepRes, recS ++ "\n" ++ f"Rec calls step: {$n$stepS$n}: $stepRes")

    val (res, s) = debug_(args)
    (res, f"Rec on args: $args {$n$s$n}: $res")
    
}

case class PrimRecSet[A <: Arity](chi: PrimRecFun[A]) {
  def contains(elem: Vector[Nat, A]): Boolean = 
    chi(elem) match {
      case ZeroNat => false 
      case _ => true
    }
}
