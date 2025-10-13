#!/bin/bash

# Test with a simpler file to see if multiple functions work
cat > /tmp/test_two_functions.moo << 'EOF'
fun first() -> Int do
    return 1
end-first

fun second() -> Int do
    return 2
end-second

fun main() -> Int do
    return 0
end-main
EOF

echo "Testing simple two functions:"
cabal run Moo -- /tmp/test_two_functions.moo

echo ""
echo "================================"
echo ""

# Test with just the first function from the failing file
cat > /tmp/test_one_function.moo << 'EOF'
fun main() -> Int do
    return 0
end-main
EOF

echo "Testing one function:"
cabal run Moo -- /tmp/test_one_function.moo
