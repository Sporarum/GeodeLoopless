import scala.compiletime.S
import scala.compiletime.error
import scala.compiletime.ops.int.<=
import annotation.showAsInfix
import scala.annotation.implicitNotFound
import scala.quoted._

trait Vector[+T, A <: Arity]{
  /*@implicitNotFound("index out of bounds")
  type Check[At <: Arity] <: T = A <= At match {
    case true => T
    //case _ => error("")
  }*/
  //type Geq[At] = A <= At
  def apply[At <: Arity](index: At): T
  def map[U](f: T => U): Vector[U, A]

  //TODO: All of those should be able to throw type errors if VNil
  def head(): T
  def tail(): Vector[T, P[A]]
  def init(): Vector[T, P[A]]
  def last(): T

  def mkString(start: String = "", sep: String = "", end: String = ""): String
  def mkString_(sep: String, end: String): String //TODO: add protected or something
  def toList(): List[T]
  def unzip[T1, T2](implicit asPair: T => (T1, T2)): (Vector[T1, A], Vector[T2, A])

  override def toString(): String = this.mkString("Vector(", ", ", ")")

  inline def +: [U >: T] (x: U): Vector[U, S[A]] = new +:(x, this)
  def :+ [U >: T] (x: U): Vector[U, S[A]] = appended(x)
  def appended[U >: T](x: U): Vector[U, S[A]]
}

object Vector{
  def apply[T](): Vector[T, 0] = VNil
  def apply[T](e0: T): Vector[T, 1] = e0 +: Vector()
  def apply[T](e0: T, e1: T): Vector[T, 2] = e0 +: Vector(e1)
}

case object VNil extends Vector[Nothing, 0]{
  def apply[At <: Arity](index: At) = throw new IllegalArgumentException(f"Acces on index ${index} of an empty Vector (This means your index was ${index+1} bigger than the size of your Vector)")
  def map[U](f: Nothing => U) = VNil
  def head() = throw new IllegalArgumentException(f"Empty Vector has no head")
  def tail() = throw new IllegalArgumentException(f"Empty Vector has no tail")
  def init() = throw new IllegalArgumentException(f"Empty Vector has no init")
  def last() = throw new IllegalArgumentException(f"Empty Vector has no last")
  def mkString(start: String = "", sep: String = "", end: String = ""): String = start ++ end
  def mkString_(sep: String, end: String): String = end
  def toList() = Nil
  def unzip[T1, T2](implicit asPair: Nothing => (T1, T2)) = (VNil, VNil)
  def appended[U](x: U) = x +: VNil
}

final case class +:[+T, A <: Arity](h: T, t: Vector[T,A]) extends Vector[T, S[A]]{
  def apply[At <: Arity](index: At): T = 
    if(index == 0){
      h
    }else{
      val nIndex: P[At] = minusOne(index)
      t.apply(nIndex)
    }

  def map[U](f: T => U) = f(h) +: t.map(f)
  def head() = h
  def tail() = t
  def init() =
    def fix[T, A <: Arity](v: Vector[T, S[P[A]]]): Vector[T, A] =  v.asInstanceOf[Vector[T, A]]
    t match
      case VNil => t
      case _ => val res = h +: t.init(); fix(res)
  def last() = 
    t match
      case VNil => h
      case _ => t.last()

  def mkString(start: String = "", sep: String = "", end: String = ""): String = 
    start ++ h.toString ++ t.mkString_(sep, end)
  def mkString_(sep: String, end: String): String = 
    sep ++ h.toString ++ t.mkString_(sep,end)
  def toList() = h :: t.toList()
  def unzip[T1, T2](implicit asPair: T => (T1, T2)) = 
    val (h1, h2) = asPair(h)
    val (t1, t2) = t.unzip(asPair)
    (h1 +: t1, h2 +: t2)

  
  def appended[U >: T](x: U): Vector[U, S[S[A]]] = h +: t.appended(x)
}