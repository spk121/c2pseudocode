# Installation and Usage Guide

## Requirements

- Python 3.7 or higher
- [pycparser](https://github.com/eliben/pycparser) (installed automatically by `pip`)
- A C preprocessor available as `cpp` on the `PATH` (part of GCC and Clang installations). It is not needed for `--no-cpp` or for converting strings through the Python API.

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/spk121/c2pseudocode.git
cd c2pseudocode

# Install in editable mode
pip install -e .

# Or install with development dependencies
pip install -e ".[dev]"
```

The package is not published on PyPI.

### Without Installing

`c2pseudocode.sh` runs the tool straight from the source tree. Only the dependency needs to be installed:

```bash
pip install pycparser
./c2pseudocode.sh myfile.c
```

## Command Line Usage

```
c2pseudocode [-h] [-o OUTPUT] [--no-cpp] [--only-from-file] input
```

| Option | Meaning |
|--------|---------|
| `input` | The C file to convert |
| `-o OUTPUT`, `--output OUTPUT` | Write the pseudocode to `OUTPUT` instead of stdout |
| `--only-from-file` | Only generate pseudocode for code in the input file itself, not for the headers it includes |
| `--no-cpp` | Do not run the C preprocessor. The input must be already preprocessed, or contain no preprocessor directives |
| `-h`, `--help` | Show the help message |

```bash
# Convert a C file to pseudocode
c2pseudocode myfile.c

# Save to a file
c2pseudocode myfile.c -o output.txt

# Leave out the contents of headers (recommended for files with includes)
c2pseudocode myfile.c --only-from-file

# Already preprocessed input
c2pseudocode preprocessed.i --no-cpp

# Run as a Python module
python3 -m c2pseudocode myfile.c
```

On failure, an error message is written to stderr and the exit status is 1.

## Python API

### String Conversion (No Preprocessing)

```python
from c2pseudocode import convert_c_string_to_pseudocode

c_code = """
int factorial(int n) {
    if (n <= 1)
        return 1;
    return n * factorial(n - 1);
}
"""

pseudocode = convert_c_string_to_pseudocode(c_code)
print(pseudocode)
```

Output:
```
FUNCTION factorial(n : Integer) RETURN Integer IS
BEGIN
  IF n <= 1 THEN
    RETURN 1
  END IF
  RETURN n * factorial(n - 1)
END FUNCTION
```

### File Conversion (With Preprocessing)

```python
from c2pseudocode import convert_c_file_to_pseudocode

# Convert entire file including headers
pseudocode = convert_c_file_to_pseudocode("myfile.c")

# Convert only code from the specified file (exclude headers)
pseudocode = convert_c_file_to_pseudocode("myfile.c", only_from_file=True)

# Write to file
with open("output.txt", "w") as f:
    f.write(pseudocode)
```

### Custom CPP Arguments

```python
from c2pseudocode import (
    convert_c_file_to_pseudocode,
    get_default_cpp_args
)

# Start with defaults and add custom defines
cpp_args = get_default_cpp_args()
cpp_args.extend([
    "-DMY_DEFINE=1",
    "-DDEBUG",
    "-Iinclude",
])

pseudocode = convert_c_file_to_pseudocode("myfile.c", cpp_args=cpp_args)
```

If you pass your own `cpp_args`, start from `get_default_cpp_args()` as shown: the defaults are what make GCC-specific constructs such as `__attribute__` parseable.

## API Reference

### `convert_c_string_to_pseudocode(c_code, cpp_args=None)`

Convert a C code string to pseudocode. The preprocessor is not run: the string must not contain directives such as `#include` or `#define`, and every type it uses must be declared in it.

**Parameters:**
- `c_code` (str): C source code as a string
- `cpp_args`: Ignored. Kept only for backward compatibility

**Returns:** str - Generated pseudocode

**Raises:** `pycparser.c_parser.ParseError` if C code has syntax errors

### `convert_c_file_to_pseudocode(filename, cpp_args=None, only_from_file=False)`

Convert a C file to pseudocode, running the C preprocessor on it first.

**Parameters:**
- `filename` (str): Path to C source file
- `cpp_args` (list, optional): Preprocessor arguments. Defaults to `get_default_cpp_args()`
- `only_from_file` (bool): Only convert code from the file itself, not from the headers it includes

**Returns:** str - Generated pseudocode

**Raises:**
- `FileNotFoundError` if the file does not exist
- `RuntimeError` if preprocessing fails
- `pycparser.c_parser.ParseError` if C code has syntax errors

### `convert_preprocessed_file_to_pseudocode(filename)`

Convert a C file to pseudocode without running the preprocessor. This is what `--no-cpp` uses.

**Parameters:**
- `filename` (str): Path to a C source file that is already preprocessed or contains no directives

**Returns:** str - Generated pseudocode

**Raises:**
- `FileNotFoundError` if the file does not exist
- `pycparser.c_parser.ParseError` if C code has syntax errors

### `get_default_cpp_args()`

Get the default C preprocessor arguments, which define away GCC built-ins and extensions that the parser does not understand.

**Returns:** list - List of cpp argument strings

### `PseudocodeGenerator(reduce_parentheses=False)`

The AST visitor that produces the pseudocode. Use it directly to convert a `pycparser` AST you have built yourself:

```python
import pycparser
from c2pseudocode import PseudocodeGenerator

ast = pycparser.CParser().parse("int f(int a, int b, int c) { return (a + b) * c - a * b; }")
print(PseudocodeGenerator(reduce_parentheses=True).visit(ast))
```

By default every nested binary operation is parenthesized, giving `((a + b) * c) - (a * b)`. With `reduce_parentheses=True`, parentheses that operator precedence makes unnecessary are omitted, giving `(a + b) * c - a * b`.

## Development

### Running Tests

```bash
# Run all tests
pytest

# Verbose output
pytest -v

# With coverage report
pytest --cov=c2pseudocode --cov-report=html

# Run specific test class
pytest tests/test_conversion.py::TestStringConversion -v
```

`tests/test_docs.py` converts every C example in [CONVERSION.md](CONVERSION.md) and compares the result with the pseudocode shown next to it, so a change in output must be accompanied by a change to that document.

The project layout is described in [README.md](README.md#project-structure).

## Building unifdef

The project includes the `unifdef` utility in `tools/unifdef/`:

```bash
cd tools/unifdef
make
sudo make install  # Optional
```

See [tools/unifdef/README.md](tools/unifdef/README.md) for details.
