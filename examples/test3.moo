type User def
  User(Int, Float)
end-def

fun main() -> Int do
  let x: Int := 3
  let ua: User := User(3, 3.)
  let ub: User := User(4, 4.)
  let uc: User := User(5, 5.)
  let users: List(User) := Cons(ua, Nil)

  return x + 2
end-main