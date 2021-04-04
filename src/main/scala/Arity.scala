import scala.compiletime.S
//import scala.compiletime.ops.int._
import scala.compiletime.testing.{Error => TypeError}
import scala.compiletime.testing.typeCheckErrors
import scala.annotation.implicitNotFound
//import scala.language.experimental.dependent
//import scala.util.Not


type Arity = Int //& Singleton

@implicitNotFound("Type does not have a predecessor")
type P[A <: Arity] <: Arity = A match {
  case S[b] => b
  //case 0 => typeCheckErrors("")
}

/*type Simplify[T <: Arity] = T match {
  
}*/


type <=[A1 <: Arity, A2 <: Arity] = A1 match
  case S[a1] => A2 match
    case S[a2] => a1 <= a2
    case 0 => false
  case 0 => true


def minusOne[A <: Arity](x: A): P[A] = (x-1).asInstanceOf[P[A]]

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
