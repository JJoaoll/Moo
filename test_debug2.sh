#!/bin/bash

# Test with just the main function from the failing file
cat > /tmp/test_main_only.moo << 'EOF'
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
EOF

echo "Testing main function only:"
cabal run Moo -- /tmp/test_main_only.moo 2>&1 | head -20

echo ""
echo "================================"
echo ""

# Now test with main + first additional function
cat > /tmp/test_main_plus_one.moo << 'EOF'
type User def
    User(Int, Float)
end-def

fun main() -> Int do
    return 0
end-main

fun acressAllSals(users: List(User), accress: Float) -> List(User) do
    let newusers := Nil
    for user in users do
        match user with
            case User(age, rent) do
                newusers := Cons(User(age, rent+acress), Nil) ++ newusers 
            end-case
        end-match
    end-for

    return newusers
end-acressAllSals
EOF

echo "Testing main + acressAllSals:"
cabal run Moo -- /tmp/test_main_plus_one.moo 2>&1 | head -20
