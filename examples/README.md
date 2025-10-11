# Moo Language Examples

This directory contains example programs for the Moo programming language, organized into two categories:

## Valid Examples (`valid/`)

These programs should compile and run successfully:

1. **01_simple_function.moo** - Basic function with parameters and return
2. **02_user_type_list.moo** - User-defined type with list construction
3. **03_factorial_recursive.moo** - Recursive factorial with pattern matching
4. **04_float_operations.moo** - Floating-point arithmetic
5. **05_global_variables.moo** - Global variable usage with @global
6. **06_safe_division_option.moo** - Division with Option type for error handling
7. **07_user_type_simple.moo** - Simple user type definition and usage
8. **08_recursive_type.moo** - Recursive type definition (Foo/Loo)
9. **09_type_definition.moo** - Basic type definition example
10. **10_nat_type_unification.moo** - Natural numbers (Peano) with type unification
11. **11_user_list_complex.moo** - Complex list operations with user types (for/match)
12. **12_user_list_match.moo** - User list with pattern matching (work in progress)
13. **13_user_type_function.moo** - User type passed to functions

## Invalid Examples (`invalid/`)

These programs should fail with specific errors:

1. **01_type_mismatch_char_to_int.moo** - Type error: assigning char to Int variable
2. **02_wrong_function_end_name.moo** - Syntax error: end-bad instead of end-badend
3. **03_wrong_main_end_name.moo** - Syntax error: end-maino instead of end-main
4. **04_division_by_zero.moo** - Runtime error: division by zero
5. **05_variable_out_of_scope.moo** - Scope error: variable 'y' not in scope
6. **06_type_mismatch_user_plus_int.moo** - Type error: cannot add User type to Int
7. **07_wrong_return_type.moo** - Type error: returning Float when Int expected

## Running Examples

To run a valid example:
```bash
cabal run Moo examples/valid/01_simple_function.moo
```

To test an invalid example (should show error):
```bash
cabal run Moo examples/invalid/01_type_mismatch_char_to_int.moo
```
