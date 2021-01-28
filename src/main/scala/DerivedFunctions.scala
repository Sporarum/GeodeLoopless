
def identity[A <: Arity](n: A): PrimRecFun[A] = ???

def subDotOne: PrimRecFun[1] = UserDefined("subDotOne", Rec(Const(Nat(0)),Proj(0)))

def subDot: PrimRecFun[2] = UserDefined("subDot", Rec(Proj(0), Comp(subDotOne, Vector(Proj(2)))))

def add: PrimRecFun[2] = UserDefined("add", Rec(Proj(0), Succ on Proj(2)))

//f(z *: X) = 0 if for all t <= z: (t *: X) not in A
def boundedMin[A <: Arity](set: PrimRecSet[A]): PrimRecFun[A] = ???