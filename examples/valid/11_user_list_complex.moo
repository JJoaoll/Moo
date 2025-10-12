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

    us := acressAllSals(us, acress)
    let usrec: List(User) := acressAllSalsRec(us, acress)

    print(us)
    print(usrec)

    // missing strings in the lang..
    match us == usrec with
        case True do
            print(0)
        end-case

        case False do
            print(-1)
        end-case
    end-match

    return 0
end-main

fun acressAllSals(users: List(User), accress: Float) -> List(User) do
    let newusers := Nil
    for user in users do
        match user with
            case User(age, rent) do
                // silly thing just for trying the cat
                newusers := Cons(User(age, rent+acress), Nil) ++ newusers 
            end-case
        end-match
    end-for

    return newusers
end-acressAllSals

fun acressAllSalsRec(users: List(User), accress: Float) -> List(User) do
    match users with 
        case Nil do 
            return Nil
        end-case

        case Cons(u, us) do
            match u with
                case User(age, rent) do
                    return Cons(User(age, rent+acress), acressAllSalsRec(us, acress))
                end-case
            end-match

        end-case
    end-match

end-acressAllSalsRec



