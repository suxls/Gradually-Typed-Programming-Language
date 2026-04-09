# Gradually-Typed Programming Language

A typed, modular object-oriented language interpreter built in OCaml. XTR features structural typing via **shapes**, a **CESK abstract machine** runtime, and **sound linking** for typed module imports.

Programs are written as S-expressions and pass through a multi-stage pipeline: parsing, validation, type checking, sound linking, and execution.

## Language Features

- **Arithmetic**: `+`, `/`, `==` on numeric values
- **Variables & Definitions**: `(def x 42.0)` binds a variable
- **Control Flow**: `if0` (branch on zero) and `while0` (loop while zero)
- **Classes & Objects**: `(class Point (x y) (method getX () (this --> x)))` defines a class with fields and methods
- **Field Access & Mutation**: `(obj --> fieldName)` reads a field, `(obj --> fieldName = expr)` writes it
- **Method Calls**: `(obj --> methodName (arg1 arg2))` dispatches a method
- **Type Checking (`isa`)**: `(obj isa ClassName)` returns `0.0` if true, `1.0` otherwise
- **Modules**: `(module Name ...)` defines an untyped module, `(tmodule Name ... Shape)` defines a typed module
- **Imports**: `(import ModuleName)` for untyped, `(timport ModuleName Shape)` for typed imports with structural shape contracts
- **Structural Typing (Shapes)**: Types are either `Number` or a shape `((FieldType*) (MethodType*))` describing the expected structure

## Architecture

```
Input (.ss)
  │
  ▼
┌─────────┐    ┌────────────┐    ┌─────────────┐    ┌──────────────┐
│  Parse   │───▶│  Validate  │───▶│  Typecheck   │───▶│  Wellformed   │
│ (S-exp   │    │ (dupes,    │    │ (structural  │    │  Conversion   │
│  → AST)  │    │  imports,  │    │  typing)     │    │  (strip errs) │
└─────────┘    │  undecl.)  │    └─────────────┘    └──────────────┘
               └────────────┘                              │
                                                           ▼
                                              ┌──────────────────┐
                                              │   Sound Linker   │
                                              │  (synthesize     │
                                              │   tmodule copies │
                                              │   for timports)  │
                                              └──────────────────┘
                                                       │
                                                       ▼
                                              ┌──────────────────┐
                                              │     Linker       │
                                              │  (resolve class  │
                                              │   names across   │
                                              │   modules)       │
                                              └──────────────────┘
                                                       │
                                                       ▼
                                              ┌──────────────────┐
                                              │  CESK Machine    │
                                              │  (interpret the  │
                                              │   linked program)│
                                              └──────────────────┘
                                                       │
                                                       ▼
                                                    Output
```

### Source Modules

| Module | Purpose |
|--------|---------|
| `system.ml` | AST types, S-expression parser, validation passes (duplicates, imports, undeclared variables) |
| `typecheck.ml` | Structural type checker using shape tables and type environments |
| `wellformed_system.ml` | Strips error variants from validated AST; sound linker that synthesizes typed module copies for `timport`s |
| `linker.ml` | Resolves class names to module-qualified names across the system |
| `cesk.ml` | CESK abstract machine interpreter with support for objects, proxies, and closures |
| `name.ml` | Identifier type with keyword/number rejection |
| `exceptions.ml` | Custom exception types for runtime and unexpected errors |
| `xtr_lib.ml` | Pipeline orchestration: chains all stages from stdin to stdout |

## Building

### Prerequisites

- OCaml >= 5.0
- opam
- dune >= 3.20

### Setup

```bash
# Install dependencies
opam install . --deps-only

# Build
dune build

# Run tests
dune runtest
```

### Usage

XTR reads an S-expression program from stdin and prints the result to stdout:

```bash
echo '((def x 3.0) (def y 4.0) (x + y))' | dune exec xtr
# Output: 7.0
```

Or pipe a file:

```bash
dune exec xtr < examples/classes_and_objects.ss
# Output: 7.0
```

## Examples

### Arithmetic

```scheme
((def a 10.0) (def b 5.0) (def sum (a + b)) (def two 2.0) (sum / two))
;; => 7.5
```

### Classes and Methods

```scheme
(
  (tmodule Geometry
    (class Point (x y)
      (method getX () (this --> x))
      (method getY () (this --> y))
      (method sum ()
        (def px (this --> x))
        (def py (this --> y))
        (px + py)))
    (((x Number) (y Number))
     ((getX () Number) (getY () Number) (sum () Number))))

  (timport Geometry
    (((x Number) (y Number))
     ((getX () Number) (getY () Number) (sum () Number))))

  (def a 3.0) (def b 4.0)
  (def p (new Point (a b)))
  (p --> sum ())
)
;; => 7.0
```

### Typed Module Chain

```scheme
(
  (tmodule M (class D () (method m () 666.0)) (() ((m () Number))))
  (tmodule N
    (import M)
    (class C () (method id () 1.0) (method m () (new D ())))
    (() ((id () Number) (m () (() ((m () Number)))))))
  (import M) (import N)
  (def c (new C ()))
  (def d (c --> m ()))
  (def e (d --> m ()))
  (def f (c --> id ()))
  (e + f)
)
;; => 667.0
```

See the [`examples/`](examples/) directory for more.

## Grammar

```
System     ::= (Module* Import* Def* Stmt* Expr)
Module     ::= (module Name Import* Class)
             | (tmodule Name MixedImport* Class Shape)
Import     ::= (import Name)
MixedImport::= (import Name) | (timport Name Shape)
Class      ::= (class Name (Field*) Method*)
Method     ::= (method Name (Param*) Def* Stmt* Expr)
Def        ::= (def Name Expr)
Stmt       ::= (Name = Expr)
             | (if0 Expr Block Block)
             | (while0 Expr Block)
             | (Name --> Field = Expr)
Block      ::= Stmt | (block Def* Stmt+)
Expr       ::= Number | Variable
             | (Var + Var) | (Var / Var) | (Var == Var)
             | (new ClassName (Var*))
             | (Var --> FieldName)
             | (Var --> MethodName (Var*))
             | (Var isa ClassName)
Shape      ::= ((FieldType*) (MethodType*))
FieldType  ::= (FieldName Type)
MethodType ::= (MethodName (Type*) Type)
Type       ::= Number | Shape
```

## Error Handling

The interpreter reports errors at each pipeline stage:

| Error | Stage |
|-------|-------|
| `"parser error"` | Malformed S-expression or grammar violation |
| `"duplicate class name"` / `"duplicate module name"` / `"duplicate method, field, or parameter name"` | Duplicate validation |
| `"bad import"` | Invalid import (e.g., importing untyped module without `timport`) |
| `"undeclared variable error"` | Undeclared variable in scope |
| `"type error"` | Structural type mismatch |
| `"run-time error"` | Runtime failure (division by zero, type mismatch on reassignment, shape nonconformance, etc.) |

## License

MIT
