
type Foo def 
  Foo(Int)
  Loo(Foo)
end-def

type Nat def
  Zero
  Succ(Nat)
end-def

fun main() -> Int do
  let x: Int := 3
  let loo: Foo := Loo(Foo(x))
  
  // putting literals after Succ
  let two0: Nat := Succ(Succ(Zero))

  //using vars instead
  let zero: Nat := Zero
  let one:  Nat := Succ(zero)
  let two1: Nat := Succ(one)

  print(two0)
  print(two1)

  return 0
end-main
