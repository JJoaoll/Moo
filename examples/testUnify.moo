
type Foo def 
  Foo(Int)
  Loo(Foo)
end-def


fun main() -> Int do
  let foo: Foo := Foo(3)

  let loo: Foo := Loo(foo)


  print(foo)

  return 0
end-main
