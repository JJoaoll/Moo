
type User def
  User(Int, Float)
end-def

<const> users: List(User) := Nil

fun upSaleAll(users: List(User), acress: Float) -> List(User) do
    let us: List(User) := Nil
    for user in users do
        match user with
            case User(name, age, rent) do
                us := Cons(User(name, age, rent+acress), us)
            end-case
        end-match 
    end-for

    return us
end-upSaleAll

fun main() -> Int do
  print (upSaleAll(<users>, 2.))

  return 0
end-main