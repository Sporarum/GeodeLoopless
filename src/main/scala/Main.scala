
object Main {

  def main(args: Array[String]): Unit = {
    val proj: PrimRecFun[2] = Proj(3)
    val res = proj(Vector(Nat(1), Nat(4)))
    println(res)
  }

}
