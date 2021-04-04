import org.junit.Test
import org.junit.Before
import org.junit.Assert._
import scala.util.Random


class PrimRecFunTest {
  @Before def initialize() = {

  }

  @Test def `Const(0) returns 0`(): Unit = {
    assertEquals(Nat(0), Const(Nat(0))(Vector(Nat(4))))
    assertEquals(Nat(0), Const(Nat(0))(Vector(Nat(4),Nat(2))))
    assertEquals(Nat(0), Const(Nat(0))(Nat(1) +: Nat(2) +: Nat(3) +: VNil))
  }

  @Test def `Const(rand) returns rand`(): Unit = {
    val rand = Nat(Random.nextInt(100))
    val rand1 = Nat(Random.nextInt(100))
    val rand2 = Nat(Random.nextInt(100))
    val rand3 = Nat(Random.nextInt(100))
    assertEquals(rand, Const(rand)(rand1 +: VNil))
    assertEquals(rand, Const(rand)(rand2 +: rand3 +: VNil))
    assertEquals(rand, Const(rand)(rand1 +: rand2 +: rand3 +: VNil))
  }

  @Test def `Proj(0) returns 1st element`(): Unit = {
    val rand0 = Nat(Random.nextInt(100))
    val rand1 = Nat(Random.nextInt(100))
    val rand2 = Nat(Random.nextInt(100))
    val v = rand0 +: rand1 +: rand2 +: VNil

    assertEquals(rand0, Proj(0)(v))
  }

  @Test def `Proj(1) returns 2st element`(): Unit = {
    val rand0 = Nat(Random.nextInt(100))
    val rand1 = Nat(Random.nextInt(100))
    val rand2 = Nat(Random.nextInt(100))
    val v = rand0 +: rand1 +: rand2 +: VNil

    assertEquals(rand1, Proj(1)(v))
  }

  @Test def `Proj(2) returns 3st element`(): Unit = {
    val rand0 = Nat(Random.nextInt(100))
    val rand1 = Nat(Random.nextInt(100))
    val rand2 = Nat(Random.nextInt(100))
    val v = rand0 +: rand1 +: rand2 +: VNil

    assertEquals(rand2, Proj(2)(v))
  }

  @Test def `Succ(rand) returns rand + 1`(): Unit = {
    val int = Random.nextInt(100)
    assertEquals(Nat(int + 1), Succ(Vector(Nat(int))))
  }

  //TODO: tests for Rec and Comp

}