# 🐄 Moo Programming Language

[![Haskell](https://img.shields.io/badge/Haskell-9.8.4-5e5086?style=flat&logo=haskell)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/badge/Build-Passing-brightgreen.svg)]()

**Moo** is a statically-typed functional programming language with imperative features, built in Haskell. It combines the safety of static typing with the expressiveness of algebraic data types and pattern matching.

## ✨ Features

### 🔧 **Core Language Features**
- **Static Type System** with type inference
- **Algebraic Data Types** with custom constructors
- **Pattern Matching** with comprehensive case analysis
- **First-class Functions** with proper closures
- **Imperative Constructs** (variables, loops, I/O)
- **Generic Types** with type parameter unification

### 🏗️ **Language Constructs**
- **Data Types**: Integers, Characters, Floats, Booleans, Lists, Custom ADTs
- **Expressions**: Literals, Variables, Binary/Unary Operations, Function Calls
- **Statements**: Variable Declaration/Assignment, I/O, Control Flow, Pattern Matching
- **Functions**: Parameter lists, return types, local scoping
- **Pattern Matching**: Literal, Variable, Wildcard, and Constructor patterns

### 🧮 **Built-in Types**
```moo
-- Basic types
TInt, TChar, TFloat, TBool

-- Composite types  
TList a, TOption a

-- Custom algebraic data types
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
-- Factorial function with pattern matching
fun factorial(n: Int) -> Int {
    match n {
        0 -> 1,
        n -> n * factorial(n - 1)
    }
}

-- List processing with custom types
data Option a = None | Some a

fun safeHead(list: List Int) -> Option Int {
    match list {
        [] -> None,
        x::xs -> Some x
    }
}

-- Main function
fun main() -> Int {
    let result = factorial(5);
    print(result);
    return 0;
}
```

## 📖 Language Syntax

### Variable Declaration & Assignment
```moo
let x: Int = 42;           -- Declaration with type annotation
x = x + 1;                 -- Assignment
```

### Function Definitions
```moo
fun add(x: Int, y: Int) -> Int {
    return x + y;
}
```

### Pattern Matching
```moo
match expression {
    pattern1 -> result1,
    pattern2 -> result2,
    _ -> defaultResult
}
```

### Control Flow
```moo
-- While loops
while condition {
    statements;
}

-- For loops
for item in list {
    print(item);
}
```

### Custom Data Types
```moo
data BinaryTree a = 
    | Empty 
    | Node a (BinaryTree a) (BinaryTree a)
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
- Supports primitive types (`TInt`, `TChar`, `TFloat`, `TBool`)
- Generic types with type variables (`TVar`)
- Algebraic data types (`TData`)
- Built-in composite types (`TList`, `TOption`)

#### 🔍 **Semantic Analyser** (`Analyser.*`)
- **Program Analysis**: Validates complete programs
- **Type Checking**: Ensures type safety across all constructs
- **Scope Management**: Handles local/global variable resolution
- **Pattern Analysis**: Validates pattern matching completeness

#### 🏃 **Function Analysis** (`Analyser.Fun.*`)
- **Local Scoping**: Variable declaration and lookup
- **Type Inference**: Expression type inference within functions
- **Control Flow**: Statement sequence validation
- **Return Analysis**: Ensures proper return type matching

## 🔬 Technical Details

### Type Checking Algorithm
The Moo compiler implements a **bidirectional type checking** system:

1. **Context Construction**: Build global context with types, functions, constants
2. **Type Validation**: Ensure all type definitions are well-formed
3. **Expression Inference**: Infer types for expressions with unification
4. **Statement Checking**: Validate statements preserve type invariants
5. **Pattern Analysis**: Ensure pattern completeness and type consistency

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

### Current Status: **Core Type System ✅**
- [x] AST definitions
- [x] Type checking infrastructure
- [x] Semantic analysis
- [x] Pattern matching validation
- [x] Function analysis
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

*Moo - Where functional meets practical* 🐄

</div>
