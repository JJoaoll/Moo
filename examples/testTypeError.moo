type User def
  User(Int, Float)
end-def

fun main() -> Int do
  let x: Int := 10
  let u: User := User(1, 1.0)

  return x + u
end-main