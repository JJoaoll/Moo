type User def
    User(Int, Float)
end-def

fun main() -> Int do
    let u0: User := User(0, 4.) 
    let u1: User := User(1, 3.) 
    let u2: User := User(2, 2.) 
    let u3: User := User(3, 1.) 

    let us: List(User) := 
        Cons(u0, 
        Cons(u1,
        Cons(u2,
        Cons(u3,
        Nil))))

    let acress: Float := scan!(Float)

    us := accSals(us, acress)
    //let us_rec: List(User) := acressAllSalsRec(us, acress)
    
    print (us)

    return 0
end-main

fun accSals(users: List(User), accress: Float) -> List(User) do
    let new_users: List(User) := Nil

    for user in users do
        match user with
            case User(age, rent) do

            end-case

        end-match

    end-for

    return new_users

end-accSals

