


trait Nat(val int: Int)
case object ZeroNat            extends Nat(0)
case class  SuccNat(nat: Nat)  extends Nat(nat.int + 1)


object Nat {

  def apply(n: Int): Nat =
    if(n == 0){
      ZeroNat
    }else if( n > 0){
      SuccNat(apply(n-1))
    }else{
      ???
    }
    
}