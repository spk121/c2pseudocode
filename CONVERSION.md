# Conversion Rules

This document describes how `c2pseudocode` renders each C construct as Ada-like pseudocode, and which C details are simplified or lost along the way.

For installation, command-line options and the Python API, see [INSTALL.md](INSTALL.md).

The converter uses `pycparser` to parse C code into an Abstract Syntax Tree (AST), then generates pseudocode from the tree. Every C/pseudocode pair below is checked against the converter's real output by `tests/test_docs.py`.

## Design Goals

1. **Readability** - Pseudocode should be easy to understand without C knowledge
2. **Ada-like** - Uses Ada conventions which are clear and explicit
3. **Documentation-ready** - Output suitable for inclusion in design documents
4. **Loss of implementation details** - Focuses on logic, not low-level details

The output is pseudocode, not Ada: it is not meant to compile.

## Data Types

| C Type | Pseudocode Type |
|--------|----------------|
| `int` | `Integer` |
| `double` | `Float64` |
| `char` | `Character` |
| `char *`, `char []` | `String` |
| `void` return type | none; the function becomes a `PROCEDURE` |
| Anything else (`float`, `long`, `short`, `unsigned int`, `size_t`, `uint8_t`, custom types, ...) | Unchanged |

Qualifiers and storage classes (`const`, `volatile`, `static`, `extern`, `register`) are dropped. Pointer declarators are dropped too, so an `int *` is shown as `Integer`; see [Pointers](#pointers).

## Operators

| C Operator | Pseudocode | Description |
|------------|-----------|-------------|
| `=` | `:=` | Assignment |
| `==` | `EQUALS` | Equality comparison |
| `!=` | `NOT EQUALS` | Inequality comparison |
| `<`, `<=`, `>`, `>=` | Unchanged | Ordering comparison |
| `&&` | `AND` | Logical AND |
| `\|\|` | `OR` | Logical OR |
| `!` | `NOT` | Logical NOT |
| `+`, `-`, `*`, `/` | Unchanged | Arithmetic |
| `%` | `MOD` | Remainder |
| `&` | `BITWISE_AND` | Bitwise AND |
| `\|` | `BITWISE_OR` | Bitwise OR |
| `^` | `BITWISE_XOR` | Bitwise exclusive OR |
| `~` | `BITWISE_NOT` | Bitwise complement |
| `<<` | `SHIFT_LEFT` | Left shift |
| `>>` | `SHIFT_RIGHT` | Right shift |
| `x++`, `++x` | `x := x + 1` | Increment |
| `x--`, `--x` | `x := x - 1` | Decrement |
| `x += y` (and `-=`, `*=`, `/=`) | `x := x + y` | Compound assignment |
| `x %= y` | `x := x MOD y` | Compound remainder |
| `x &= y` (and `\|=`, `^=`, `<<=`, `>>=`) | `x := x BITWISE_AND y` | Compound bitwise assignment |
| `c ? a : b` | `(IF (c) THEN (a) ELSE (b))` | Conditional expression |
| `sizeof(x)` | `SIZE_OF(x)` | Size of a type or object |
| `(type) x` | `x` | Casts are not shown |
| `s.m`, `p->m` | `s.m`, `p.m` | Member access |
| `*p`, `&x` | `p`, `x` | See [Pointers](#pointers) |

`NULL`, which the preprocessor expands to `((void *)0)`, is shown as `NULL`.

Nested binary operations are always parenthesized, whether or not C's precedence rules require it:

**C:**
```c
ok = a > 0 && b % 2 == 0;
```

**Pseudocode:**
```
ok := (a > 0) AND ((b MOD 2) EQUALS 0)
```

Note that `x++` used inside a larger expression is rendered as an assignment in place, which no longer says whether the old or the new value is used.

## Control Structures

### If Statement

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

An `else if` chain becomes `ELSIF` branches of a single `IF`:

**C:**
```c
if (x > 0) {
    sign = 1;
} else if (x < 0) {
    sign = -1;
} else {
    sign = 0;
}
```

**Pseudocode:**
```
IF x > 0 THEN
  sign := 1
ELSIF x < 0 THEN
  sign := -1
ELSE
  sign := 0
END IF
```

### For Loop

The converter recognizes simple counting loops and converts them to a more readable format. A loop is recognized when it initializes one variable, compares that variable with `<`, `<=`, `>` or `>=`, and steps it with `++`, `--`, `+=` or `-=` in the matching direction.

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

**Range notation.** Ranges use mathematical interval notation and are always written with the low bound first. A square bracket means the bound is included, a round bracket means it is excluded.

| C loop | Pseudocode | Values of `i` |
|--------|-----------|---------------|
| `for (i = 0; i < 10; i++)` | `FOR i IN RANGE [0, 10) LOOP` | 0, 1, ..., 9 |
| `for (i = 0; i <= 10; i++)` | `FOR i IN RANGE [0, 10] LOOP` | 0, 1, ..., 10 |
| `for (i = 5; i >= 0; i--)` | `FOR i IN REVERSE RANGE [0, 5] LOOP` | 5, 4, ..., 0 |
| `for (i = 5; i > 0; i--)` | `FOR i IN REVERSE RANGE (0, 5] LOOP` | 5, 4, ..., 1 |

`REVERSE` marks a loop that counts down: it starts at the high bound and works towards the low bound, as in Ada's `for I in reverse Low .. High`.

**With a step value:**

**C:**
```c
for (i = 0; i < 20; i += 2) {
    total += values[i];
}
```

**Pseudocode:**
```
FOR i IN RANGE [0, 20) STEP 2 LOOP
  total := total + values[i]
END LOOP
```

**Descending loop:**

**C:**
```c
for (i = 5; i >= 0; i--) {
    total += values[i];
}
```

**Pseudocode:**
```
FOR i IN REVERSE RANGE [0, 5] LOOP
  total := total + values[i]
END LOOP
```

**General for loop.** For loops that don't match the counting pattern, the C-style three-part header is preserved:

**C:**
```c
for (i = 0; i < 10 && arr[i] != 0; i++) {
    sum += arr[i];
}
```

**Pseudocode:**
```
FOR i := 0; (i < 10) AND (arr[i] NOT EQUALS 0); i := i + 1 LOOP
  sum := sum + arr[i]
END LOOP
```

A `for` with neither an initializer nor a step is shown as a `WHILE` loop, or as a bare `LOOP` when it has no condition either:

**C:**
```c
for (;;) {
    poll();
}
```

**Pseudocode:**
```
LOOP
  poll()
END LOOP
```

### While Loop

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

### Do-While Loop

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

### Break, Continue and Goto

| C | Pseudocode |
|---|-----------|
| `break;` (in a loop) | `EXIT` |
| `continue;` | `CONTINUE` |
| `goto label;` | `GOTO label` |
| `label:` | `label:` |
| `;` (empty statement) | `DO_NOTHING` |

### Switch Statement

**C:**
```c
switch (value) {
    case 1:
        count = 1;
        break;
    case 2:
        count = 2;
        break;
    default:
        count = 0;
        break;
}
```

**Pseudocode:**
```
CASE value IS
  WHEN 1 =>
    count := 1
  WHEN 2 =>
    count := 2
  WHEN OTHERS =>
    count := 0
END CASE
```

The `break` that ends a case is not shown, because a `WHEN` branch never continues into the next one.

Case labels that share the same statements are merged into one branch. When a case does run on into the next one because it has statements but no `break`, the pseudocode says so explicitly:

**C:**
```c
switch (value) {
    case 1:
    case 2:
        count = 1;
        break;
    case 3:
        count = 3;
    case 4:
        total = total + count;
        break;
}
```

**Pseudocode:**
```
CASE value IS
  WHEN 1 | 2 =>
    count := 1
  WHEN 3 =>
    count := 3
    -- FALLS THROUGH to the next WHEN branch
  WHEN 4 =>
    total := total + count
END CASE
```

A `break` that is not the last statement of its case, for instance one inside an `if`, is shown as `EXIT`; there it means "leave the `CASE`", not "leave the enclosing loop".

## Functions and Procedures

A function that returns a value becomes a `FUNCTION`; one that returns `void` becomes a `PROCEDURE`.

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

Local variable declarations stay where they are in the body, after `BEGIN`.

Function prototypes (declarations without a body) are not shown. Only function definitions appear in the output.

## Type Definitions

### Struct

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

A `union` is rendered the same way with `UNION` and `END UNION`. A bit-field width is shown after the member's type, as in `flags : Integer : 3`.

### Enum

Enumerator values are shown only where the C code gives them explicitly.

**C:**
```c
enum Color {
    RED,
    GREEN = 5,
    BLUE
};
```

**Pseudocode:**
```
TYPE Color IS ENUM
  RED,
  GREEN = 5,
  BLUE
END ENUM
```

### Other Typedefs

**C:**
```c
typedef unsigned long counter_t;
```

**Pseudocode:**
```
TYPE counter_t IS unsigned long
```

## Declarations

**C:**
```c
int x = 10;
int arr[5];
char initial = 'a';
const char *name = "none";
```

**Pseudocode:**
```
x : Integer := 10
arr[5] : Integer
initial : Character := 'a'
name : String := "none"
```

An initializer list is written in parentheses: `int a[3] = {1, 2, 3};` becomes `a[3] : Integer := (1, 2, 3)`.

## Standard Library Calls

Calls to common C library functions are replaced with descriptive names. The arguments are passed through unchanged unless noted. Calls to any other function are shown as they are.

### stdio.h

| C | Pseudocode |
|---|-----------|
| `printf(fmt, ...)` | `PRINT("...")` |
| `fprintf(fp, fmt, ...)` | `PRINT(fp, "...")` |
| `sprintf(buf, fmt, ...)`, `snprintf(buf, n, fmt, ...)` | `FORMAT_STRING(buf, "...")` (the size is dropped) |
| `scanf(fmt, ...)` | `READ_INPUT("...")` |
| `fscanf(fp, fmt, ...)` | `READ_FROM_FILE(fp, "...")` |
| `sscanf(str, fmt, ...)` | `PARSE_STRING(str, "...")` |
| `fopen` / `fclose` | `OPEN_FILE` / `CLOSE_FILE` |
| `fread` / `fwrite` | `READ` / `WRITE` |
| `fgets(buf, n, fp)` | `READ_LINE(buf, fp)` (the size is dropped) |
| `fputs` | `WRITE_LINE` |
| `fgetc`, `getchar` | `READ_CHAR` |
| `fputc`, `putchar` | `WRITE_CHAR` |
| `fseek(fp, off, whence)` | `SET_FILE_POSITION(fp, off, whence)` with `SEEK_SET`, `SEEK_CUR` and `SEEK_END` shown as `START`, `CURRENT` and `END` |
| `ftell` | `GET_FILE_POSITION` |
| `rewind` | `REWIND` |
| `feof` / `ferror` / `clearerr` | `AT_END` / `HAS_ERROR` / `CLEAR_ERROR` |
| `remove` / `rename` | `DELETE_FILE` / `RENAME_FILE` |
| `tmpfile` | `CREATE_TEMP_FILE` |

When the format is a string literal, each conversion specifier is replaced by the argument it formats, written in braces:

**C:**
```c
printf("Total for %s: %d\n", name, total);
scanf("%d", &count);
```

**Pseudocode:**
```
PRINT("Total for {name}: {total}\n")
READ_INPUT("{count}")
```

Width, precision and other formatting details of the specifier are not shown.

### stdlib.h

| C | Pseudocode |
|---|-----------|
| `malloc` / `calloc` / `realloc` / `free` | `ALLOCATE` / `ALLOCATE_ARRAY` / `REALLOCATE` / `DEALLOCATE` |
| `exit` / `abort` | `EXIT_PROGRAM` / `ABORT_PROGRAM` |
| `rand` / `srand` | `RANDOM_INTEGER` / `SET_RANDOM_SEED` |
| `atoi` / `atol` / `atof` | `STRING_TO_INTEGER` / `STRING_TO_LONG` / `STRING_TO_FLOAT` |
| `strtol(s, end, base)` | `PARSE_INTEGER(s, base)` |
| `strtod(s, end)` | `PARSE_FLOAT(s)` |
| `abs` | `ABSOLUTE_VALUE` |

### string.h

| C | Pseudocode |
|---|-----------|
| `strlen` | `STRING_LENGTH` |
| `strcpy`, `strncpy` | `COPY_STRING` |
| `strcat`, `strncat` | `APPEND_STRING` |
| `strcmp`, `strncmp` | `COMPARE_STRINGS` |
| `strchr` / `strrchr` | `FIND_CHAR` / `FIND_LAST_CHAR` |
| `strstr` | `FIND_STRING` |
| `memcpy` / `memmove` | `COPY_MEMORY` / `MOVE_MEMORY` |
| `memset` / `memcmp` | `FILL_MEMORY` / `COMPARE_MEMORY` |

### math.h

| C | Pseudocode |
|---|-----------|
| `sqrt` | `SQUARE_ROOT` |
| `fabs` | `ABSOLUTE_VALUE` |
| `pow` / `exp` | `POWER` / `EXPONENTIAL` |
| `log` / `log2` / `log10` | `NATURAL_LOG` / `LOG2` / `LOG10` |
| `ceil` / `floor` / `round` | `CEILING` / `FLOOR` / `ROUND` |
| `sin` / `cos` / `tan` | `SINE` / `COSINE` / `TANGENT` |
| `asin` / `acos` / `atan` / `atan2` | `ARC_SINE` / `ARC_COSINE` / `ARC_TANGENT` / `ARC_TANGENT2` |

The substitution is by name only. A function of your own that happens to be called `remove` or `round` is renamed as well.

## Simplifications and Limitations

The pseudocode deliberately leaves out low-level detail. Review the output before putting it in a design document, paying particular attention to the following.

### Pointers

Pointer syntax is removed entirely:

- A pointer declaration shows only the pointed-to type: `int *p` is `p : Integer`.
- Dereference and address-of are dropped: `*p = 3` is `p := 3`, and `p = &x` is `p := x`.
- `p->member` is `p.member`.

**C:**
```c
void reset(int *count, struct Node *node) {
    *count = 0;
    node->next = NULL;
}
```

**Pseudocode:**
```
PROCEDURE reset(count : Integer, node : RECORD Node) IS
BEGIN
  count := 0
  node.next := NULL
END PROCEDURE
```

This reads well for the common case of a pointer used as an output parameter or a reference to a record. It also means the pseudocode cannot distinguish assigning to a pointer from assigning through it, so code that does pointer arithmetic or re-seats pointers needs a hand-written explanation.

### Macros and Conditional Compilation

The C preprocessor runs before conversion, so the pseudocode shows the code after macro expansion:

- A constant defined with `#define SIZE 16` appears as `16`, not as `SIZE`. Use an `enum` or a `const` variable if the name should survive.
- A function-like macro appears as its expansion.
- `#if` and `#ifdef` blocks are resolved with whatever macros are defined; only the selected branch appears. Run `unifdef` first to choose the branches explicitly.
- Comments are removed.

### Other C Features

1. **Function prototypes** - Not shown; only definitions are
2. **Type qualifiers and storage classes** - `const`, `volatile`, `static`, `extern` are dropped
3. **Casts** - Dropped; only the operand is shown
4. **Function pointers** - Rendered in a C-like declarator form, e.g. `(handler)(Integer) : Integer`
5. **Array and function typedefs** - Keep their declarator form, e.g. `TYPE row_t[5] : Integer`
6. **Inline assembly** - Not supported
7. **Compiler extensions** - GCC attributes and built-ins are removed by the default preprocessor arguments; other compilers may need their own (see [README.md](README.md#handling-compiler-specific-types))
8. **Standard newer than C99** - Limited to what `pycparser` can parse

## Related Tools

- **unifdef** - Remove conditional compilation directives before conversion (included in `tools/unifdef/`)
- **pycparser** - The underlying C parser (https://github.com/eliben/pycparser)

## Authors

- Based on pycparser by Eli Bendersky
- Pseudocode generator modifications by Mike Gran and contributors

Licensed under the BSD 3-Clause License; see [LICENSE](LICENSE).
