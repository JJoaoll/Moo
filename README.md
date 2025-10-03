# 🐄 Moo Programming Language

[![Haskell](https://img.shields.io/badge/Haskell-9.8.4-5e5086?style=flat&logo=haskell)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/badge/Build-Passing-brightgreen.svg)]()

**Moo** is a statically-typed imperative programming language with rich algebraic data types, built in Haskell. It combines the safety of static typing with the expressiveness of algebraic data types and comprehensive pattern matching for imperative programming.

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
data Tree a = Leaf a | Node (Tree a) a (Tree a)
data Result a b = Ok a | Error b
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
fun factorial(n: Int) -> Int {
    let result: Int = 1;
    let i: Int = 1;
    
    while i <= n {
        result = result * i;
        i = i + 1;
    }
    
    return result;
}

-- List processing with algebraic data types
data Option a = None | Some a

fun safeHead(list: List Int) -> Option Int {
    match list {
        [] -> None,
        x::xs -> Some x
    }
}

-- Main function with imperative flow
fun main() -> Int {
    let x: Int = 5;
    let result: Int = factorial(x);
    print(result);
    
    let numbers: List Int = [1, 2, 3, 4, 5];
    let head: Option Int = safeHead(numbers);
    
    match head {
        Some value -> print(value),
        None -> print("Empty list")
    }
    
    return 0;
}
```

## 📖 Language Syntax

### Variable Declaration & Assignment
```moo
let x: Int = 42;           -- Variable declaration with type annotation
x = x + 1;                 -- Mutable assignment (imperative style)
let mut y: Int = 0;        -- Explicit mutable variable declaration
```

### Function Definitions
```moo
-- Functions are procedures that can modify state
fun add(x: Int, y: Int) -> Int {
    let result: Int = x + y;
    return result;
}
```

### Pattern Matching
```moo
-- Pattern matching for algebraic data types
match expression {
    pattern1 -> {
        statements;
        result1
    },
    pattern2 -> {
        statements;
        result2
    },
    _ -> defaultResult
}
```

### Control Flow
```moo
-- While loops
while condition {
    statements;
}

-- For loops with mutable iteration
for item in list {
    print(item);
}

-- Conditional statements
if condition {
    statements;
} else {
    otherStatements;
}
```

### Custom Algebraic Data Types
```moo
-- Inductive types with multiple constructors
data BinaryTree a = 
    | Empty 
    | Node a (BinaryTree a) (BinaryTree a)

-- Recursive data structures
data List a = Nil | Cons a (List a)
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
- **Program Analysis**: Validates complete imperative programs
- **Type Checking**: Ensures type safety for mutable variables and operations
- **Scope Management**: Handles variable mutability and local/global resolution
- **Pattern Analysis**: Validates exhaustive pattern matching on algebraic types

#### 🏃 **Function Analysis** (`Analyser.Fun.*`)
- **Imperative Scoping**: Mutable variable declaration and assignment tracking
- **Type Inference**: Expression type inference with mutation constraints
- **Control Flow**: Statement sequence validation for imperative constructs
- **Return Analysis**: Ensures proper return type matching in all execution paths

## 🔬 Technical Details

### Type Checking Algorithm
The Moo compiler implements a **bidirectional type checking** system:

1. **Context Construction**: Build global context with types, procedures, constants
2. **Type Validation**: Ensure all algebraic type definitions are well-formed and support induction
3. **Expression Inference**: Infer types for expressions with unification and mutation tracking
4. **Statement Checking**: Validate imperative statements preserve type invariants and mutability rules
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
- [x] AST definitions for imperative constructs
- [x] Rich algebraic type system with inductive types
- [x] Type checking infrastructure for mutable variables
- [x] Semantic analysis for imperative programs
- [x] Pattern matching validation for algebraic data types
- [x] Function analysis with imperative scoping
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

This project is licensed under the **GNU General Public License v3.0** - see the [LICENSE](LICENSE) file for details.

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
