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

### Filter to Only User Code

For files with `#include` statements (like `<stdio.h>`), use `--only-from-file` to exclude system headers and only show pseudocode for your actual source code:

```bash
python3 c-to-pseudocode.py input.c --only-from-file -o output.txt
```

This is useful when converting files that include system libraries, as it avoids generating hundreds of lines of system library pseudocode.

**Example:**
```bash
# Without filtering - includes all of stdio.h and dependencies
$ python3 c-to-pseudocode.py myprogram.c | wc -l
487 lines

# With filtering - only your code
$ python3 c-to-pseudocode.py myprogram.c --only-from-file | wc -l
23 lines
```

### Without C Preprocessor

If your file is already preprocessed or doesn't use includes/defines:

```bash
python3 c-to-pseudocode.py input.c --no-cpp
```

### With unifdef

For production use, you may want to use `unifdef` to remove debug code first:

```bash
# Preprocess with unifdef, then convert the result
unifdef -DDEBUG=0 input.c > preprocessed.c
python3 c-to-pseudocode.py --no-cpp preprocessed.c -o output.txt
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
| `==` | `EQUALS` | Equality comparison |
| `!=` | `NOT EQUALS` | Inequality comparison |
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

The converter recognizes simple counting loops and converts them to a more readable format.

**Simple Counting Loop (Recognized):**

**C:**
```c
for (i = 0; i < 10; i++) {
    sum += i;
}
```

**Pseudocode:**
```
FOR i IN RANGE [0, 10) LOOP
  sum := sum + i
END LOOP
```

**Range Notation:**
- `[start, end)` - Ascending loop from start (inclusive) to end (exclusive)
- `[start, end]` - Ascending loop from start to end (both inclusive)
- `(start, end]` - Descending loop
- `[start, end]` - Descending loop (both inclusive)

**With Step Value:**

**C:**
```c
for (i = 0; i < 20; i += 2) {
    printf("%d\n", i);
}
```

**Pseudocode:**
```
FOR i IN RANGE [0, 20) STEP 2 LOOP
  printf("%d\n", i)
END LOOP
```

**Descending Loop:**

**C:**
```c
for (i = 5; i >= 0; i--) {
    printf("%d\n", i);
}
```

**Pseudocode:**
```
FOR i IN RANGE (0, 5] LOOP
  printf("%d\n", i)
END LOOP
```

**General For Loop (Complex):**

For loops that don't match the simple counting pattern, the full C-style syntax is preserved:

**C:**
```c
for (i = 0; i < 10 && arr[i] != 0; i++) {
    sum += arr[i];
}
```

**Pseudocode:**
```
FOR i := 0; i < 10 AND arr[i] NOT EQUALS 0; i := i + 1 LOOP
  sum := sum + arr[i]
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
