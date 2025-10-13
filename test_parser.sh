#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  MOO PARSER TESTING SUITE${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Test valid examples (should parse successfully)
echo -e "${YELLOW}Testing VALID examples (should parse successfully):${NC}"
echo ""

for file in examples/valid/*.moo; do
    if [ -f "$file" ]; then
        TOTAL=$((TOTAL + 1))
        filename=$(basename "$file")
        echo -n "Testing $filename... "
        
        # Run the parser and capture output
        output=$(cabal run Moo -- "$file" 2>&1)
        exit_code=$?
        
        # Check if parse was successful
        if echo "$output" | grep -q "✓ Parse successful!"; then
            echo -e "${GREEN}✓ PASSED${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}✗ FAILED${NC}"
            echo -e "${RED}Expected: Parse successful${NC}"
            echo -e "${RED}Got:${NC}"
            echo "$output" | head -20
            echo ""
            FAILED=$((FAILED + 1))
        fi
    fi
done

echo ""
echo -e "${YELLOW}Testing INVALID examples (should fail to parse or analyze):${NC}"
echo ""

# Test invalid examples (should fail)
for file in examples/invalid/*.moo; do
    if [ -f "$file" ]; then
        TOTAL=$((TOTAL + 1))
        filename=$(basename "$file")
        echo -n "Testing $filename... "
        
        # Run the parser and capture output
        output=$(cabal run Moo -- "$file" 2>&1)
        exit_code=$?
        
        # Check if parse or analysis failed (as expected)
        if echo "$output" | grep -q -E "(Parse error|Analysis error|Runtime error)" || [ $exit_code -ne 0 ]; then
            echo -e "${GREEN}✓ PASSED (correctly rejected)${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}✗ FAILED${NC}"
            echo -e "${RED}Expected: Error${NC}"
            echo -e "${RED}Got: Success (should have failed!)${NC}"
            echo ""
            FAILED=$((FAILED + 1))
        fi
    fi
done

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  RESULTS${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "Total tests: ${TOTAL}"
echo -e "${GREEN}Passed: ${PASSED}${NC}"
echo -e "${RED}Failed: ${FAILED}${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed! 🎉${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed! 😞${NC}"
    exit 1
fi
