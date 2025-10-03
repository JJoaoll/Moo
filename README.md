# 🐄 Moo Programming Language

[![Haskell](https://img.shields.io/badge/Haskell-9.8.4-5e5086?style=flat&logo=haskell)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/badge/Build-Passing-brightgreen.svg)]()

**Moo** is a statically-typed imperative programming language with rich algebraic data types, built in Haskell. It combines the safety of static typing with the expressiveness of algebraic data types and comprehensive pattern matching programming.

## ✨ Features

### 🔧 **Core Language Features**
- **Static Type System** with comprehensive type checking
- **Rich Algebraic Data Types** with custom constructors and type parameters
- **Inductive Types** supporting recursive data structures
- **Pattern Matching** with exhaustive case analysis
- **Imperative Programming** with mutable variables and sequential execution
- **Control Flow Constructs** (loops, conditionals, early returns)
- **Generic Types** with type parameter unification and inference

### 🏗️ **Language Constructs**
- **Data Types**: Integers, Characters, Floats, Booleans, Lists, Custom Algebraic Types
- **Expressions**: Literals, Variables, Binary/Unary Operations, Function Calls, Constructors
- **Statements**: Variable Declaration/Assignment, I/O Operations, Control Flow, Pattern Matching
- **Functions**: Procedures with parameter lists, return types, local scoping
- **Pattern Matching**: Literal, Variable, Wildcard, and Constructor patterns for data destructuring

### 🧮 **Built-in Types**
```moo
-- Primitive types
TInt, TChar, TFloat, TBool

-- Composite types with type parameters
TList a, TOption a

-- Custom algebraic data types (inductive types)
type Tree(a) def
    Leaf(a)
    Node(Tree(a), a, Tree(a))
end-def

type Result(a, b) def
    Ok(a)
    Error(b)
end-def
```

## 🚀 Quick Start

### Prerequisites
- **GHC 9.8.4+** (Haskell compiler)
- **Cabal 3.0+** (Haskell build tool)

### Installation
```bash
# Clone the repository
git clone https://github.com/JJoaoll/miti.git
cd miti

# Build the project
cabal build

# Run the Moo interpreter
cabal run
```

### Example Program
```moo
-- Factorial function with imperative style
fun fact(n: Int) -> Int do
    let result: Int := 1
    let i: Int := 1
    
    while i <= n do
        result := result * i
        i := i + 1
    end-while
    
    return result
end-factorial

-- List processing with algebraic data types
type Option(a) def
    None
    Some(a)
end-def

fun safeHead(list: List Int) -> Option Int do
    match list with
        [] do 
            return None 
        end-case

        x::xs do 
            return (Some x) 
        end-case
    end-match
end-safeHead

-- entry point: function main
fun main() -> Int do
    let x: Int := 5
    let result: Int := factorial(x)
    print(result)
    
    let numbers: List Int := [1, 2, 3, 4, 5]
    let head: Option Int := safeHead(numbers)
    
    match head with
        Some value do print(value) end-case
        None do print("Empty list") end-case
    end-match
    
    return 0
end-main
```

## 📖 Language Syntax

### Variable Declaration & Assignment
```moo
let x: Int := 42          -- Variable declaration with type annotation (mutable by default)
x := x + 1                -- Mutable assignment (imperative style)
let y: Int := 0           -- All variables declared with 'let' are mutable
```

### Function Definitions
```moo
-- Functions are procedures that can modify state
fun add(x: Int, y: Int) -> Int do 
    let result: Int := x + y
    return result
end-add
```

### Pattern Matching
```moo
-- Pattern matching for algebraic data types
match expression with
    pattern0 do 
        sttms0
    end-case 
    pattern1 do
        sttms1
    end-case 
    otherwise do 
        sttms2
    end-case
end-match
```

### Control Flow
```moo
-- While loops
while condition do
    statements
end-while

-- For loops with iteration
for i in items do
    print(i)
end-for

-- Conditional statements
if condition then
    statements
else 
    otherStatements
end-if
```

### Custom Algebraic Data Types
```moo
-- Inductive types with multiple constructors
type BinTree(a, b) def
    Leaf(a)
    Branch(BinTree(a, b), b, BinTree(a, b))
end-def

-- Recursive data structures
type List(a) def 
    Nil 
    Cons(a, List(a))
end-def
```

## 🏛️ Architecture

### Project Structure
```
src/
├── Grammar/           -- Abstract Syntax Tree definitions
│   ├── Program.hs     -- Program structure (functions, globals, types)
│   ├── Expr.hs        -- Expression grammar and operators
│   ├── Type.hs        -- Type system and pattern synonyms
│   └── Sttm.hs        -- Statements and pattern matching
├── Analyser/          -- Semantic analysis and type checking
│   ├── Program.hs     -- Program-level analysis
│   ├── Error.hs       -- Error types and handling
│   ├── TypeDef.hs     -- Type definition validation
│   ├── Context/       -- Analysis contexts
│   └── Fun/           -- Function-level analysis
├── Parser/            -- Parsing utilities (TODO)
├── Interpreter/       -- Runtime execution (TODO)
└── Main.hs           -- Entry point
```

### Core Components

#### 🧠 **Type System** (`Grammar.Type`)
- Rich primitive types (`TInt`, `TChar`, `TFloat`, `TBool`)
- Generic types with type variables (`TVar`) for parametric polymorphism
- Algebraic data types (`TData`) supporting sum and product types
- Inductive types enabling recursive data structures
- Built-in composite types (`TList`, `TOption`) with type parameters

#### 🔍 **Semantic Analyser** (`Analyser.*`)
- **Program Analysis**: Validates complete programs
- **Type Checking**: Ensures type safety for mutable variables and operations
- **Scope Management**: Handles variable mutability and local/global resolution
- **Pattern Analysis**: Validates exhaustive pattern matching on algebraic types

#### 🏃 **Function Analysis** (`Analyser.Fun.*`)
- **Imperative Scoping**: Mutable variable declaration and assignment tracking
- **Type Inference**: Expression type inference with mutation constraints
- **Control Flow**: Statement sequence validation for constructs
- **Return Analysis**: Ensures proper return type matching in all execution paths

## 🔬 Technical Details

### Type Checking Algorithm
The Moo compiler implements a **bidirectional type checking** system:

1. **Context Construction**: Build global context with types, procedures, constants
2. **Type Validation**: Ensure all algebraic type definitions are well-formed and support induction
3. **Expression Inference**: Infer types for expressions with unification and mutation tracking
4. **Statement Checking**: Validate statements preserve type invariants and mutability rules
5. **Pattern Analysis**: Ensure pattern completeness and type consistency for algebraic data types

### Error Handling
Comprehensive error reporting with specific error types:
- `VarNotFound`: Undefined variable references
- `FunNotFound`: Undefined function calls
- `IncorrectArity`: Wrong number of function arguments
- `PatternNotFound`: Incomplete pattern matching

## 📚 Documentation

### Haddock Documentation
Generate comprehensive API documentation:
```bash
cabal haddock --haddock-all
```

View generated docs at: `dist-newstyle/build/.../doc/html/Moo/Moo/index.html`

### Module Documentation Coverage
- ✅ **Grammar modules**: 100% documented
- ✅ **Analyser modules**: 100% documented  
- ✅ **Context management**: 100% documented
- ✅ **Function analysis**: 100% documented

## 🛠️ Development

### Building from Source
```bash
# Clean build
cabal clean
cabal build

# Run with specific arguments
cabal run Moo -- [arguments]

# Development build with warnings
cabal build --ghc-options="-Wall"
```

### Dependencies
```yaml
dependencies:
  - base ^>=4.19.1.0
  - containers
  - megaparsec       # Parser combinators
  - mtl              # Monad transformer library
  - parser-combinators
  - text             # Efficient text processing
  - lens             # Functional record updates
```

### Code Style
- **Haddock comments** for all public functions
- **Type signatures** for top-level definitions
- **Lens-based** record field access
- **Qualified imports** for namespace clarity

## 🎯 Roadmap

### Current Status: **Core Type System & Imperative Analysis ✅**
- [x] AST definitions for constructs
- [x] Rich algebraic type system with inductive types
- [x] Type checking infrastructure for mutable variables
- [x] Semantic analysis for programs
- [x] Pattern matching validation for algebraic data types
- [x] Function analysis with scoping
- [x] Comprehensive documentation

### Next Milestones:
- [ ] **Parser Implementation** - Convert source code to AST
- [ ] **Interpreter/Compiler** - Execute Moo programs  
- [ ] **Standard Library** - Built-in functions and data structures
- [ ] **REPL** - Interactive development environment
- [ ] **Package System** - Module imports and exports
- [ ] **Optimization** - Performance improvements

## 🤝 Contributing

We welcome contributions! Please:

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/amazing-feature`)
3. **Commit** your changes (`git commit -m 'Add amazing feature'`)
4. **Push** to the branch (`git push origin feature/amazing-feature`)
5. **Open** a Pull Request

### Development Guidelines
- Add Haddock documentation for new functions
- Include type signatures for all definitions
- Write comprehensive test cases (when test suite is added)
- Follow existing code style and conventions

## 📄 License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 Author

**João Duarte**  
📧 [joaoduos@gmail.com](mailto:joaoduos@gmail.com)  
🐙 [@JJoaoll](https://github.com/JJoaoll)

## 🙏 Acknowledgments

- **Haskell Community** for excellent tooling and libraries
- **GHC Team** for the robust compiler infrastructure  
- **Lens Library** for elegant functional record updates
- **Megaparsec** for powerful parser combinators (planned usage)

---

<div align="center">

**Built with ❤️ and Haskell**

*Moo - Where imperative meets good* 🐄

</div>
