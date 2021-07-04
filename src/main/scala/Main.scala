
object Main {

  def main(args: Array[String]): Unit = {
    val proj = Proj[2](0)
    val succ: PrimRecFun[1] = Succ
    val t: P[0] = minusOne(0) //Only fails at compile time
    val vec = Nat(4) +: Nat(3) +: VNil
    val vecNumber: Vector[Double | Int, 2] = 1.0 +: 1 +: VNil
    val vecNum2 = vecNumber.map{ _ match {
      case a: Int => a.toDouble
      case a: Double => a
    }}

    val compFromApply1 = Succ(proj)
    
    def compFromApply2[A <: Arity]: PrimRecFun[A] = compFromApply1(Const[A](Nat(0)), Const[A](Nat(4)))

    val vecRev = VNil :+ "Hello World !"
    //val bug = VNil.head()

    val b = vec match{
      case h +: t => true
      case VNil => false
    }

    val res = proj(vec)
    //val res2 = proj(Nat(1) ** (Nat(4) -: VNil()))
    println(res)



    //val test = vec(4)
    //println(subDotOne.prettyDebug(Nat(10) +: VNil))
    val vec2 = Vector(Nat(6), Nat(6))
    //println(add.prettyDebug(vec2))
  }

}
