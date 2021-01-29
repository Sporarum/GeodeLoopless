
def identity[A <: Arity](n: A): PrimRecFun[A] = ???

def subDotOne: PrimRecFun[1] = UserDefined("subDotOne", Rec(Const(Nat(0)),Proj(0)))

def subDot: PrimRecFun[2] = UserDefined("subDot", Rec(Proj(0), subDotOne on Proj(2)))

def add: PrimRecFun[2] = UserDefined("add", Rec(Proj(0), Succ on Proj(2)))

def mult: PrimRecFun[2] = UserDefined("mult", Rec(Const(Nat(0)), add(Proj(2), Proj(0))))

def exp(base: Nat): PrimRecFun[1] = UserDefined("exp", Rec(Const(Nat(0)), mult(Proj(1), Const(base))))

def fact: PrimRecFun[1] = UserDefined("fact", Rec(Const(Nat(1)), mult(Proj(1), Succ on Proj(0))))


//f(z *: X) = 0 if for all t <= z: (t *: X) not in A
def boundedMin[A <: Arity](set: PrimRecSet[A]): PrimRecFun[A] = ???