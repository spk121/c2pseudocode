# C to ADA-like Pseudocode Converter

This tool converts C source code to ADA-like pseudocode for documentation purposes, particularly for Software Design Documents (SDD).

## Overview

The converter uses `pycparser` to parse C code into an Abstract Syntax Tree (AST), then generates ADA-like pseudocode that is more readable and suitable for documentation. The generated pseudocode emphasizes clarity and follows ADA naming conventions and structure.

## Installation

### Requirements

- Python 3.6 or higher
- pycparser

### Install pycparser

```bash
pip install pycparser
```

## Usage

### Basic Usage

```bash
python3 c-to-pseudocode.py input.c
```

This will output the pseudocode to stdout.

### Output to File

```bash
python3 c-to-pseudocode.py input.c -o output.txt
```

### Without C Preprocessor

If your file is already preprocessed or doesn't use includes/defines:

```bash
python3 c-to-pseudocode.py input.c --no-cpp
```

### With unifdef

For production use, you may want to use `unifdef` to remove debug code first:

```bash
unifdef -DDEBUG=0 input.c | python3 c-to-pseudocode.py --no-cpp - > output.txt
```

## Conversion Rules

### Data Types

| C Type | Pseudocode Type |
|--------|----------------|
| `int` | `Integer` |
| `char` | `String` |
| `double` | `Float64` |
| `unsigned int` | `unsigned Integer` |
| Custom types | Preserved |

### Operators

| C Operator | Pseudocode Operator | Description |
|------------|-------------------|-------------|
| `=` | `:=` | Assignment |
| `==` | `=` | Equality comparison |
| `!=` | `/=` | Inequality comparison |
| `&&` | `AND` | Logical AND |
| `||` | `OR` | Logical OR |
| `!` | `NOT` | Logical NOT |
| `++` | `:= variable + 1` | Increment |
| `--` | `:= variable - 1` | Decrement |
| `+=` | `:= variable + value` | Compound assignment |
| `-=` | `:= variable - value` | Compound subtraction |
| `*=` | `:= variable * value` | Compound multiplication |
| `/=` | `:= variable / value` | Compound division |

### Control Structures

#### If Statement

**C:**
```c
if (x > 0) {
    y = x + 1;
} else {
    y = 0;
}
```

**Pseudocode:**
```
IF x > 0 THEN
  y := x + 1
ELSE
  y := 0
END IF
```

#### For Loop

**C:**
```c
for (i = 0; i < 10; i++) {
    sum += i;
}
```

**Pseudocode:**
```
FOR i := 0; i < 10; i := i + 1 LOOP
  sum := sum + i
END LOOP
```

#### While Loop

**C:**
```c
while (n > 0) {
    n--;
}
```

**Pseudocode:**
```
WHILE n > 0 LOOP
  n := n - 1
END LOOP
```

#### Do-While Loop

**C:**
```c
do {
    sum++;
} while (sum < 100);
```

**Pseudocode:**
```
LOOP
  sum := sum + 1
EXIT WHEN NOT (sum < 100)
END LOOP
```

#### Switch Statement

**C:**
```c
switch (value) {
    case 1:
        printf("One\n");
        break;
    case 2:
        printf("Two\n");
        break;
    default:
        printf("Other\n");
        break;
}
```

**Pseudocode:**
```
CASE value IS
  WHEN 1 =>
    printf("One\n")
  WHEN 2 =>
    printf("Two\n")
  WHEN OTHERS =>
    printf("Other\n")
END CASE
```

Note: `BREAK` statements are automatically removed as they are implicit in ADA's CASE statement.

### Functions and Procedures

#### Function (Returns a Value)

**C:**
```c
int add(int a, int b) {
    return a + b;
}
```

**Pseudocode:**
```
FUNCTION add(a : Integer, b : Integer) RETURN Integer IS
BEGIN
  RETURN a + b
END FUNCTION
```

#### Procedure (Returns void)

**C:**
```c
void process(int x, int y) {
    int result;
    result = x + y;
}
```

**Pseudocode:**
```
PROCEDURE process(x : Integer, y : Integer) IS
BEGIN
  result : Integer
  result := x + y
END PROCEDURE
```

### Type Definitions

#### Struct

**C:**
```c
typedef struct {
    int x;
    int y;
} Point;
```

**Pseudocode:**
```
TYPE Point IS RECORD
  x : Integer
  y : Integer
END RECORD
```

#### Enum

**C:**
```c
enum Color {
    RED,
    GREEN,
    BLUE
};
```

**Pseudocode:**
```
TYPE Color IS ENUM
  RED = 0,
  GREEN = 1,
  BLUE = 2
END ENUM
```

### Declarations

**C:**
```c
int x = 10;
int arr[5];
Point p;
```

**Pseudocode:**
```
x : Integer := 10
arr[5] : Integer
p : Point
```

## Features Not Supported

The following C features are simplified or not fully supported:

1. **Pointers** - Pointer notation (`*`, `->`) is simplified or removed
2. **Preprocessor directives** - Use `unifdef` to handle these before conversion
3. **Complex macros** - Simple defines are converted, complex ones are not
4. **Type qualifiers** - `const`, `volatile`, `static` are removed or simplified
5. **Storage classes** - Generally removed for clarity
6. **Bit fields** - Rendered but not specially handled
7. **Inline assembly** - Not supported
8. **Function pointers** - Simplified
9. **Variable argument lists** - Rendered as `...`

## Design Goals

1. **Readability** - Pseudocode should be easy to understand without C knowledge
2. **ADA-like** - Uses ADA conventions which are clear and explicit
3. **Documentation-ready** - Output suitable for inclusion in design documents
4. **Loss of implementation details** - Focuses on logic, not low-level details

## Examples

The examples above demonstrate the conversion of various C constructs. You can test the converter with your own C files to see how they are converted.

## Related Tools

- **unifdef** - Remove conditional compilation directives before conversion
- **pycparser** - The underlying C parser (https://github.com/eliben/pycparser)

## License

BSD License (same as pycparser)

## Authors

- Based on pycparser by Eli Bendersky
- Pseudocode generator modifications by Mike Gran and contributors
