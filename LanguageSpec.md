# Language Specification

This document provides a complete specification for the programming language based on the provided example and clarified rules. It is designed to be consumable by an AI for tasks such as parsing, code generation, or analysis. The language draws inspiration from C-like syntax but with simplifications and specific constraints.

## Keywords
Reserved words that cannot be used as identifiers. These are case-sensitive.

- `const`
- `extern`
- `fn`
- `type`
- `struct`
- `while`
- `return`
- `if`
- `else`
- `elif`
- `switch`
- `break`
- `continue`
- `for`

## Built-in Types
Predefined primitive types. These are treated as identifiers in the lexer but recognized as types by the parser. `void` is an alias for `u0` and tokenized as an identifier.

- `u8` (unsigned 8-bit integer)
- `i8` (signed 8-bit integer)
- `u16` (unsigned 16-bit integer)
- `i16` (signed 16-bit integer)
- `u32` (unsigned 32-bit integer)
- `i32` (signed 32-bit integer)
- `u64` (unsigned 64-bit integer)
- `i64` (signed 64-bit integer)
- `f32` (32-bit floating point)
- `f64` (64-bit floating point)
- `u0` (void type, equivalent to `void`)
- `char` (character type, likely alias for `u8` or `i8` depending on signedness context)

User-defined types (defined via `type struct` or type aliases) follow the same identifier rules and can be used interchangeably with built-in types in declarations.

## Identifiers
Names for variables, functions, types, fields, etc.

- Case-sensitive.
- Must start with a letter (a-z, A-Z) or underscore (`_`).
- Subsequent characters: letters, digits (0-9), or underscores.
- Regex pattern: `[a-zA-Z_][a-zA-Z0-9_]*`
- Cannot match keywords.

## Literals
Directly representable values in source code.

- **String literals**: Enclosed in double quotes (e.g., `"Hello, World!"`). No escape sequences (e.g., no `\n` or `\"`). No multi-line strings.
- **Integer literals**: 
  - Decimal (e.g., `42`, `0`).
  - Hexadecimal (e.g., `0x2A`, `0xFF`).
  - Binary (e.g., `0b1010`, `0b111`).
  - No octal support.
  - Signed integers use unary `-` operator (e.g., `-42` is unary minus applied to positive literal `42`).
- No floating-point literals (e.g., no `3.14`).
- No character literals (e.g., no `'a'`).
- No boolean literals (e.g., no `true` or `false`).

## Operators
Symbols for operations, similar to C but with limitations. Multi-character operators are single tokens. Operators are tokenized without context; the parser resolves ambiguities (e.g., `*` as multiplication, dereference, or pointer declarator).

- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Relational**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logical**: `&&`, `||`, `!`
- **Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`
- **Assignment**: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- **Miscellaneous**:
  - `*` (multiplication, dereference, pointer in types)
  - `&` (bitwise AND, address-of/reference)
  - `->` (function return type arrow; not for pointer member access)
  - `.` (member access for values and pointers; compiler automatically infers and inserts dereference if the left operand is a pointer)
  - `?` `:` (ternary conditional)
  - `,` (comma separator, e.g., in parameter lists or expressions)
  - `:` (struct field initializer, labels in switch, ternary colon)

No increment (`++`) or decrement (`--`) operators.

Operator precedence and associativity follow C conventions (e.g., `*` higher than `+`, assignment right-associative).

## Punctuators and Symbols
Structural symbols for grouping and termination.

- `{` `}`: Block delimiters (for functions, loops, conditionals, structs).
- `(` `)`: Grouping expressions, function calls, parameter lists.
- `[` `]`: Array subscripts (access like `arr[0]`), array type declarations (e.g., `u8[10]`).
- `;`: Explicit statement terminator (optional if statement ends with newline).
- Newline: Implicit statement terminator (statements can end with newline instead of `;`).

## Comments
Non-executable annotations.

- Single-line: Starts with `#` and extends to the end of the line (e.g., `# This is a comment`).
- Multi-line: Starts with `#-` and ends with `-#` (e.g., `#- Multi-line comment -#`). Can span multiple lines; no nesting.

Comments are ignored by the lexer.

## Type Declarations
- **Pointers**: Declared with `*` after the base type (e.g., `char *` for pointer to char). Nestable (e.g., `char * *` for pointer to pointer to char).
- **Arrays**: Declared like C (e.g., `u8[10]` for array of 10 u8). Size must be a constant expression. Access with `[` `]` (e.g., `arr[0]`). No slices.
- **Structs**: Defined with `type struct <identifier> { <field_type> <field_name>, ... }` (e.g., `type struct Point { i32 x, i32 y }`). Fields are comma-separated; no trailing comma required.
- **Type Aliases**: Declared with `type <new_identifier> <original_type>` (e.g., `type MyInt i32`, `type StrPtr char *`). Original type can be built-in, user-defined, pointer, array, etc.

## Declarations and Definitions
- **Variables**: `<type> <identifier> [= <initializer>]` (e.g., `i32 x = 42`).
- **Constants**: Prefix with `const` (e.g., `const i32 MAX = 100`).
- **Functions**: `[<const>] fn <identifier>(<param_type> <param_name>, ...) -> <return_type> { <body> }` (e.g., `fn add(i32 a, i32 b) -> i32 { return a + b }`). `const fn` indicates pure functions (no side effects). External functions: `extern fn ...`.
- **Function Pointers**: Declared like functions but as types (e.g., `fn(i32) -> i32 myFuncPtr = add`). No anonymous lambdas or closures.
- **Struct Initializers**: `{ <field>: <value>, ... }` (e.g., `{ x: 1, y: 2 }`). Colon `:` is required; order matches declaration or can be named.
- **Parameters**: Comma-separated with types (e.g., `i32 argc, char * * argv`). No defaults or variadics.

## Statements and Control Flow
- Statements end with `;` or newline.
- Blocks: `{ ... }` for scoping; nested blocks allowed.
- **Conditionals**: `if (<expr>) { ... } [else if (<expr>) { ... }] [else { ... }]` (uses `elif` for else if).
- **Loops**: `while (<expr>) { ... }`, `for` (syntax not fully specified, assume C-like `for (init; cond; incr) { ... }` but without ++/--).
- **Switch**: `switch (<expr>) { ... }` with cases (details like C, with `break`).
- **Return**: `return [<expr>]`.
- **Break/Continue**: In loops and switch.

Indentation is not significant (whitespace-agnostic except for token separation).

## Other Rules
- **Whitespace Handling**: Spaces, tabs, newlines ignored except for token separation and newline as statement terminator.
- **Preprocessor**: None (no `#include`, `#define`, etc., beyond comments).
- **Missing Features**: No enums, unions, error handling (try/catch), anonymous lambdas/closures, parameter defaults, variadics, multi-line strings.
- **Edge Cases**: Context-sensitive tokens (e.g., `*` and `&`) resolved by parser. Compiler infers dereference for `.` on pointers (no explicit `->` for pointer access).
- **Semantics**: Similar to C where applicable (e.g., pointer arithmetic, array decay). Functions can be forward-declared with `extern`.

## Example Program
```lang
const u8 * out_cstr = "Hello, World!"
extern fn puts(char * s) -> u0
type struct string {
  u32 len,
  char * val
}
const fn strlen(char * str) -> u32 {
  char * it = str
  u32 counter = 0
  while (*it != 0) {
    counter += 1
    it += 1
  }
  return counter
}
const fn(char *) -> u32 countFunc = strlen
const string outVal = { len: strlen(out_cstr), val: out_cstr }
fn main(i32 argc, char * * argv) -> i32 {
  string test = { len: countFunc(out_cstr), val: out_cstr }
  puts(out_cstr)
  return 0
}
```

This spec is complete based on provided details; extensions may be added later.