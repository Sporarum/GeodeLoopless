import scala.compiletime.S
//import scala.compiletime.ops.int._
import scala.compiletime.ops.int.<
import scala.compiletime.testing.{Error => TypeError}
import scala.compiletime.testing.typeCheckErrors
import scala.annotation.implicitNotFound
//import scala.language.experimental.dependent
//import scala.util.Not


given true = true

type Arity = Int //& Singleton

@implicitNotFound("Type does not have a predecessor")
type P[A <: Arity] <: Arity = A match {
  case S[b] => b
  //case 0 => typeCheckErrors("")
}

/*type Simplify[T <: Arity] = T match {
  
}*/

/*
type <=[A1 <: Arity, A2 <: Arity] = A1 match
  case S[a1] => A2 match
    case S[a2] => a1 <= a2
    case 0 => false
  case 0 => true
*/
  /*
type <[A1 <: Arity, A2 <: Arity] = A1 match
  case S[a1] => A2 match
    case S[a2] => a1 < a2
    case 0 => false
  case 0 => A2 match
    case S[a2] => true
    case 0 => false
    */

/* If this definition is used, minusOneOnLessThan fails on (A1 < S[A2]), as the first match will be constant folded away
type <[A1 <: Arity, A2 <: Arity] = A2 match
  case S[a2] => A1 match
    case S[a1] => a1 < a2
    case 0 => true
  case 0 => false
  */

given minusOneOnLessThan[A1 <: Arity, A2 <: Arity](using cond: A1 < A2): (P[A1] < P[A2]) = cond.asInstanceOf[P[A1] < P[A2]]

def minusOne[A <: Arity](x: A): P[A] = if x != 0 then (x-1).asInstanceOf[P[A]] else throw new IllegalArgumentException("Cannot remove 1 from 0 (Arities cannot be negative)")

def plusOne[A <: Arity](a: A): S[A] = (a+1).asInstanceOf[S[A]]

//given [A <: Arity & Singleton](using Not[A]) as A = valueOf[A]

type Zero = 0
given Zero = 0
type Three = 3
//given Three = 3
given [A <: Arity](using a: A): S[A] = plusOne(a) //only goes up to 5 (3+2), add given of 6 to go up to 8 (6+2)


/*
type Arity[Int]
type ZeroType <: Arity[0]
type SuccType[I <: Int, X<: Arity[I]] <: Arity[I+1]
type PredType[X] = X match{
  case SuccType[_, y] => y
  case ZeroType => Unit
}
*/

/*
type ZeroType = 0
type SuccType_ <: Int
type SuccType[A <: SuccType_ | ZeroType] <: SuccType_
type PredType[X] = X match{
  case SuccType[y] => y
  case ZeroType => Unit
}

type Arity = ZeroType | SuccType_

type Arity_[I <: Int] <: Arity = (I <= 0) match {
  case true => ZeroType
  case false => SuccType[Arity_[I-1]]
}
*/
