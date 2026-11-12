# C to Pseudocode Converter

A tool to convert C source code to Ada-like pseudocode for documentation purposes.

Once upon a time, I was asked to generate pseudocode for a DO-178C Software Design Document, but, this was to backfill code that was already written. I started writing all the pseudocode by hand, but, I realized that was a waste of time. So, I wrote this tool adapted from Eli Bendersky's [https://eli.thegreenplace.net/] work on parsing C in Python.

It takes a C file and generates a sort of Ada-like pseudocode. Ada-like pseudocode is well understood in the USA Aerospace industry, at least among the greyhairs.

This project also includes a version of `unifdef` written by Tony Finch.

## What It Does

Converts C code like this:

```c
int add(int a, int b) {
    return a + b;
}
```

Into readable Ada-like pseudocode:

```
FUNCTION add(a : Integer, b : Integer) RETURN Integer IS
BEGIN
  RETURN a + b
END FUNCTION
```

## Quick Start

```bash
# Install from source
pip install -e .

# Convert a file, showing only the code from that file (not its headers)
c2pseudocode input.c --only-from-file -o output.txt
```

The tool can also be run without installing the package, as long as its one dependency, `pycparser`, is installed:

```bash
pip install pycparser
./c2pseudocode.sh input.c --only-from-file
```

Unless `--no-cpp` is given, a C preprocessor named `cpp` must be on the `PATH`.

See [INSTALL.md](INSTALL.md) for the full installation guide, all command-line options and the Python API reference.

## Key Features

- **Ada-like syntax** - Clear, explicit pseudocode format
- **Type conversion** - Common C types converted to readable names (`int` → `Integer`, `char *` → `String`)
- **Operator clarity** - C operators converted to explicit keywords (`==` → `EQUALS`, `&&` → `AND`)
- **Simplified control flow** - Uses `END IF`, `END LOOP`, `ELSIF`, `CASE ... WHEN`
- **Readable loops** - Simple counting loops become `FOR i IN RANGE [0, 10) LOOP`
- **Library calls** - Common `stdio.h`, `stdlib.h`, `string.h` and `math.h` calls are given descriptive names (`printf` → `PRINT`, `malloc` → `ALLOCATE`)
- **Smart filtering** - `--only-from-file` excludes headers, showing only your code
- **Preprocessor support** - Handles GCC extensions and system includes automatically

See [CONVERSION.md](CONVERSION.md) for the complete conversion rules, with examples, and for the list of C features that are simplified or lost in the output.

## Usage

### Command Line

```bash
# Convert a C file; pseudocode goes to stdout
c2pseudocode mycode.c

# Save output to a file
c2pseudocode mycode.c -o pseudocode.txt

# Same thing, run as a module
python3 -m c2pseudocode mycode.c
```

### Files with System Includes

By default the output covers everything the preprocessor produces, including the contents of every header. When your C file includes headers (like `<stdio.h>`), use `--only-from-file` to show only the code that is in the file itself:

```bash
c2pseudocode mycode.c --only-from-file
```

**Without `--only-from-file`**, a file that includes `<stdio.h>` generates hundreds of lines of pseudocode for the system library's types and variables (the exact number depends on your C library):

```bash
c2pseudocode tests/fixtures/t_main_no_args.c | wc -l
```

**With `--only-from-file`:**

```bash
c2pseudocode tests/fixtures/t_main_no_args.c --only-from-file
```

```
FUNCTION main() RETURN Integer IS
BEGIN
  RETURN 0
END FUNCTION
```

Types declared in headers, such as `size_t` or `FILE`, can still be used by your code; only their definitions are left out of the output.

### Already Preprocessed Files

```bash
# Skip the C preprocessor. The input must be already preprocessed,
# or contain no #include, #define or other directives.
c2pseudocode preprocessed.i --no-cpp
```

`--only-from-file` has no effect together with `--no-cpp`.

### Python API

```python
from c2pseudocode import (
    convert_c_string_to_pseudocode,
    convert_c_file_to_pseudocode,
    get_default_cpp_args,
)

# Convert a C code string (the preprocessor is not run on strings)
c_code = """
int add(int a, int b) {
    return a + b;
}
"""
print(convert_c_string_to_pseudocode(c_code))

# Convert a file (with preprocessing), excluding its headers
print(convert_c_file_to_pseudocode("myfile.c", only_from_file=True))

# Custom cpp arguments
custom_args = get_default_cpp_args()
custom_args.append("-DMY_DEFINE=1")
print(convert_c_file_to_pseudocode("myfile.c", cpp_args=custom_args))
```

### Handling Compiler-Specific Types

For this to work, the script needs to be able to identify every type. Some compilers, like gcc, have types that it considers fundamental, like `__builtin_va_list`, or has C grammar that is not standard, like `__attribute__`.  All the common ones I've run across with `gcc` are converted to more familiar C constructs via `get_default_cpp_args()`.  If you are using a different compiler or C library, you may need to customize the cpp arguments when calling the API functions.

## Workflow

### Recommended: Filter to Your Code Only

```bash
c2pseudocode mycode.c --only-from-file -o pseudocode.txt
```

### With Conditional Compilation Removal

The preprocessor resolves every `#if` and `#ifdef` using the macros that happen to be defined. To choose explicitly which conditional code is documented, for example to drop debug code, run `unifdef` first:

1. Use `unifdef` to remove debug code and conditional compilation
2. Run `c2pseudocode` to generate pseudocode
3. Include the pseudocode in your documentation

```bash
# Remove the code that is conditional on DEBUG
unifdef -UDEBUG input.c > cleaned.c

# Convert only the code from cleaned.c (not its headers)
c2pseudocode cleaned.c --only-from-file -o output.txt
```

`unifdef` is included in `tools/unifdef/`; see [tools/unifdef/README.md](tools/unifdef/README.md) for how to build it.

## Testing

```bash
# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest
```

## Project Structure

```
c2pseudocode/
├── src/c2pseudocode/            # Main package
│   ├── __init__.py              # Public API exports
│   ├── __main__.py              # CLI entry point
│   ├── api.py                   # Core API functions
│   └── pseudocode_generator.py  # AST visitor for code generation
├── tests/                       # Test suite
│   ├── fixtures/                # Test C files
│   ├── test_conversion.py       # API, file and CLI tests
│   ├── test_docs.py             # Checks the examples in CONVERSION.md
│   ├── test_grammar.py          # C grammar tests
│   ├── test_stdio_functions.py  # stdio.h substitutions
│   └── test_stdlib_string_math.py  # stdlib.h, string.h, math.h substitutions
├── tools/unifdef/               # unifdef utility (separate C program)
├── c2pseudocode.sh              # Runs the tool from the source tree
├── pyproject.toml               # Python packaging configuration
├── requirements.txt             # Runtime dependencies
├── README.md                    # This file
├── INSTALL.md                   # Installation, CLI and API reference
├── CONVERSION.md                # Conversion rules and limitations
└── LICENSE                      # BSD 3-Clause license
```

## License

BSD 3-Clause License. See [LICENSE](LICENSE).

The bundled `unifdef` is by Tony Finch and carries its own BSD-style license; see the source files in `tools/unifdef/`.
