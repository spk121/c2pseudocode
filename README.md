# C to Pseudocode Converter

A tool to convert C source code to ADA-like pseudocode for documentation purposes.

Once upon a time, I was asked to generate pseudocode for a DO-178C Software Design Document, but, this was to backfill code that was already written. I started writing all the pseudocode by hand, but, I realized that was a waste of time. So, I wrote this tool adapted from Eli Bendersky's [https://eli.thegreenplace.net/] work on parsing C in Python.

It takes a C file and generates a sort of ADA-like pseudocode. ADA-like pseudocode is well understood in the USA Aerospace industry, at least among the greyhairs.

This project also includes a version of `unifdef` written by Tony Finch.

## Quick Start

### Installation

```bash
# Install from source
pip install -e .

# Or just install dependencies
pip install pycparser
```

### Command Line Usage

```bash
# Convert a C file (shows everything including system headers)
c2pseudocode input.c

# Or run as a module
python3 -m c2pseudocode input.c

# Convert only the code from your file (recommended for most cases)
c2pseudocode input.c --only-from-file -o output.txt
```

### Python API Usage

```python
from c2pseudocode import convert_c_string_to_pseudocode, convert_c_file_to_pseudocode

# Convert C code string to pseudocode
c_code = """
int add(int a, int b) {
    return a + b;
}
"""
pseudocode = convert_c_string_to_pseudocode(c_code)
print(pseudocode)

# Convert a C file to pseudocode
pseudocode = convert_c_file_to_pseudocode("myfile.c", only_from_file=True)
print(pseudocode)
```

## What It Does

Converts C code like this:

```c
int add(int a, int b) {
    return a + b;
}
```

Into readable ADA-like pseudocode:

```
FUNCTION add(a : Integer, b : Integer) RETURN Integer IS
BEGIN
  RETURN a + b
END FUNCTION
```

## Key Features

- **ADA-like syntax** - Clear, explicit pseudocode format
- **Type conversion** - C types converted to readable names (int → Integer)
- **Operator clarity** - C operators converted to explicit keywords (== → EQUALS, && → AND)
- **Simplified control flow** - Uses END IF, END LOOP, etc. for clarity
- **Documentation-ready** - Output suitable for Software Design Documents
- **Smart filtering** - `--only-from-file` excludes system headers, showing only your code
- **Preprocessor support** - Handles GCC extensions and system includes automatically

## Usage

### Command Line

```bash
# Convert a simple C file
c2pseudocode mycode.c

# Save output to a file
c2pseudocode mycode.c -o pseudocode.txt
```

### Python API

```python
from c2pseudocode import (
    convert_c_string_to_pseudocode, 
    convert_c_file_to_pseudocode,
    get_default_cpp_args
)

# Convert C code string (without preprocessing)
c_code = """
int add(int a, int b) {
    return a + b;
}
"""
pseudocode = convert_c_string_to_pseudocode(c_code)
print(pseudocode)

# Convert a file (with preprocessing)
pseudocode = convert_c_file_to_pseudocode("myfile.c", only_from_file=True)
print(pseudocode)

# Custom cpp arguments
custom_args = get_default_cpp_args()
custom_args.append("-DMY_DEFINE=1")
pseudocode = convert_c_file_to_pseudocode("myfile.c", cpp_args=custom_args)
```

### Files with System Includes

When your C file includes system headers (like `<stdio.h>`), use `--only-from-file` to filter out the system library code and only show your own code:

```bash
# Only show pseudocode for code in mycode.c, not from <stdio.h>, <stdlib.h>, etc.
c2pseudocode mycode.c --only-from-file

# Example: A file with #include <stdio.h> that would otherwise generate 
# hundreds of lines of system library code now shows only your functions
```

**Without `--only-from-file`:**
```bash
$ c2pseudocode tests/fixtures/t_main_no_args.c | wc -l
239  # Includes all of stdio.h and its dependencies
```

**With `--only-from-file`:**
```bash
$ c2pseudocode tests/fixtures/t_main_no_args.c --only-from-file
FUNCTION main() RETURN Integer IS
BEGIN
  RETURN 0
END FUNCTION
```

### Advanced Options

```bash
# Don't use C preprocessor (for already preprocessed files)
c2pseudocode preprocessed.i --no-cpp

# Combine options
c2pseudocode mycode.c --only-from-file -o output.txt
```

## Testing

Run the test suite with pytest:

```bash
# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Run with coverage
pytest --cov=c2pseudocode --cov-report=html
```

## Documentation

See [CONVERSION.md](CONVERSION.md) for complete conversion rules and examples.

### Handling Compiler-Specific Types

For this to work, the script needs to be able to identify every type. Some compilers, like gcc, have types that it considers fundamental, like `__builtin_va_list`, or has C grammar that is not standard, like `__attribute__`.  All the common ones I've run across with `gcc` are converted to more familiar C constructs via `get_default_cpp_args()`.  If you are using a different compiler or C library, you may need to customize the cpp arguments when calling the API functions.

## Project Structure

```
c2pseudocode/
├── src/c2pseudocode/        # Main package
│   ├── __init__.py          # Public API exports
│   ├── __main__.py          # CLI entry point
│   ├── api.py               # Core API functions
│   └── pseudocode_generator.py  # AST visitor for code generation
├── tests/                   # Test suite
│   ├── fixtures/            # Test C files
│   └── test_conversion.py   # Pytest test cases
├── tools/unifdef/           # unifdef utility (separate C program)
├── pyproject.toml           # Modern Python packaging configuration
├── requirements.txt         # Runtime dependencies
└── README.md                # This file
```
- `CONVERSION.md` - Complete documentation of conversion rules

## Workflow

### Recommended: Filter to Your Code Only

```bash
# Best practice: Only generate pseudocode for your actual code
python3 c-to-pseudocode.py mycode.c --only-from-file -o pseudocode.txt
```

### With Conditional Compilation Removal

1. (Optional) Use `unifdef` to remove debug code and conditional compilation
2. Run `c-to-pseudocode.py` to generate pseudocode
3. Include the pseudocode in your documentation

Example with unifdef:

```bash
# Preprocess with unifdef to remove conditional code
unifdef -DDEBUG=0 input.c > cleaned.c

# Convert only the code from cleaned.c (not system headers)
python3 c-to-pseudocode.py cleaned.c --only-from-file -o output.txt
```

## License

BSD License
