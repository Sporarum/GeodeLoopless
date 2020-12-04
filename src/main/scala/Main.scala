
object Main {

  def main(args: Array[String]): Unit = {
    val proj = Proj[0](0) //Should fail
    val succ: PrimRecFun[1] = Succ
    val t: P[0] = minusOne(0) //Should fail, but at least is not -1
    val vec = Nat(4) *: Nat(3) *: VNil()
    val res = proj(Vector())
    //val res2 = proj(Nat(1) ** (Nat(4) -: VNil()))
    println(res)
  }

}
