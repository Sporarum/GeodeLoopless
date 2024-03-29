import scala.compiletime.S
import scala.compiletime.error
import scala.compiletime.ops.int.<=
import annotation.showAsInfix
import scala.annotation.implicitNotFound
import scala.quoted._

trait Vector[+T, A <: Arity]:

  val length: A = valueOf[A]

  /*@implicitNotFound("index out of bounds")
  type Check[At <: Arity] <: T = A <= At match {
    case true => T
    //case _ => error("")
  }*/
  //type Geq[At] = A <= At
  def apply[At <: Arity](index: At): T
  def map[U](f: T => U): Vector[U, A]

  //TODO: All of those should be able to throw type errors if VNil
  def head_ : T
  def tail_ : Vector[T, P[A]]
  def init_ : Vector[T, P[A]]
  def last_ : T

  def reverse: Vector[T, A]
  def fold[U >: T](z: U)(op: (U,U) => U): U = foldLeft(z)(op)
  def foldLeft[U](z: U)(op: (U, T) => U): U
  def foldRight[U](z: U)(op: (T, U) => U): U = reverse.foldLeft(z){case (u, t) => op(t, u)}
  def scan[U >: T](z: U)(op: (U,U) => U): Vector[U, S[A]] = scanLeft(z)(op)
  def scanLeft[U](z: U)(op: (U, T) => U): Vector[U, S[A]]
  def scanRight[U](z: U)(op: (T, U) => U): Vector[U, S[A]] = reverse.scanLeft(z){case (u, t) => op(t, u)}.reverse
  def mkString(start: String = "", sep: String = "", end: String = ""): String
  def mkString_(sep: String, end: String): String //TODO: add protected or something
  def toList(): List[T]
  def unzip[T1, T2](implicit asPair: T => (T1, T2)): (Vector[T1, A], Vector[T2, A])

  def zip[U](other: Vector[U, A]): Vector[(T,U), A]
  def zipWithIndex: Vector[(T,Int), A] = zipWithIndex_(0)
  def zipWithIndex_(offset: Int): Vector[(T,Int), A] //TODO: add protected, seems to cause problems
  override def toString(): String = this.mkString("Vector(", ", ", ")")

  inline def +: [U >: T] (x: U): Vector[U, S[A]] = new +:(x, this)
  def :+ [U >: T] (x: U): Vector[U, S[A]] = appended(x)
  def appended[U >: T](x: U): Vector[U, S[A]]

  //TODO: Remove me
  def toSPA(): Vector[T, S[P[A]]]
end Vector

object Vector:

  def apply[T](): Vector[T, 0] = VNil
  def apply[T](e0: T): Vector[T, 1] = e0 +: Vector()
  def apply[T](e0: T, e1: T): Vector[T, 2] = e0 +: Vector(e1)

  def filled[T, A <: Arity](times: A, elem: T): Vector[T,A] = filled(times)(_ => elem)

  def filled[T, A <: Arity](times: A)(f: Arity => T): Vector[T,A] =
    if times == 0 then 
      //A will always equal 0 (or Int)
      VNil.asInstanceOf[Vector[Nothing, A]] 
    else
      val tMin1: P[A] = minusOne(times)
      val init = filled[T,P[A]](tMin1)(f)
      //if not times-1, will result in vect(0) != f(0)
      (init :+ f(tMin1)).toA()



case object VNil extends Vector[Nothing, 0]:
  def apply[At <: Arity](index: At) = throw new IllegalArgumentException(f"Acces on index ${index} of an empty Vector (This means your index was ${index+1} bigger than the size of your Vector)")
  def map[U](f: Nothing => U) = VNil
  def head_ = throw new IllegalArgumentException(f"Empty Vector has no head")
  def tail_ = throw new IllegalArgumentException(f"Empty Vector has no tail")
  def init_ = throw new IllegalArgumentException(f"Empty Vector has no init")
  def last_ = throw new IllegalArgumentException(f"Empty Vector has no last")
  def reverse = VNil
  def scanLeft[U](z: U)(op: (U, Nothing) => U) = z +: VNil
  def foldLeft[U](z: U)(op: (U, Nothing) => U) = z
  def mkString(start: String = "", sep: String = "", end: String = ""): String = start ++ end
  def mkString_(sep: String, end: String): String = end
  def toList() = Nil
  def zip[U](other: Vector[U, 0]) = VNil
  def unzip[T1, T2](implicit asPair: Nothing => (T1, T2)) = (VNil, VNil)
  def zipWithIndex_(offset: Int) = VNil
  def appended[U](x: U) = x +: VNil

  def toSPA() = throw new IllegalArgumentException(f"Attempt to cast toSPA on VNil, this would allow to create a Vector[T,P[0]]!")


final case class +:[+T, A <: Arity](h: T, t: Vector[T,A]) extends Vector[T, S[A]]:
  def apply[At <: Arity](index: At): T = 
    if index == 0
    then h
    else
      val nIndex: P[At] = minusOne(index)
      t.apply(nIndex)

  def map[U](f: T => U) = f(h) +: t.map(f)
  def head_ = h
  def tail_ = t

  def init_ =
    t match
      case VNil => t
      case _ => val res = h +: t.init_; res.toA()
  
  def last_ = 
    t match
      case VNil => h
      case _ => t.last_
  def reverse = t.reverse :+ h

  def foldLeft[U](z: U)(op: (U, T) => U) = t.foldLeft(op(z,h))(op)

  def scanLeft[U](z: U)(op: (U, T) => U) = z +: t.scanLeft(op(z,h))(op)

  def mkString(start: String = "", sep: String = "", end: String = ""): String = 
    start ++ h.toString ++ t.mkString_(sep, end)
  def mkString_(sep: String, end: String): String = 
    sep ++ h.toString ++ t.mkString_(sep,end)
  def toList() = h :: t.toList()
  def unzip[T1, T2](implicit asPair: T => (T1, T2)) = 
    val (h1, h2) = asPair(h)
    val (t1, t2) = t.unzip(asPair)
    (h1 +: t1, h2 +: t2)
  def zip[U](other: Vector[U, S[A]]) = (h, other.head) +: t.zip(other.tail)
  def zipWithIndex_(offset: Int) = (h, offset) +: t.zipWithIndex_(offset + 1)
  def appended[U >: T](x: U): Vector[U, S[S[A]]] = h +: t.appended(x)

  def toSPA() = this
  
end +:

extension[T, A <: Arity] (v: Vector[T, S[P[A]]])
  def toA(): Vector[T, A] = v.asInstanceOf[Vector[T, A]]

extension[T, A <: Arity] (v: Vector[T, S[A]])
  def head = v.head_
  def tail = v.tail_
  def init = v.init_
  def last = v.last_
