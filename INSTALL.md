# Installation and Usage Guide

## Installation

### From Source (Development)

```bash
# Clone the repository
git clone https://github.com/spk121/c2pseudocode.git
cd c2pseudocode

# Install in editable mode
pip install -e .

# Or install with development dependencies
pip install -e ".[dev]"
```

### Using pip (when published)

```bash
pip install c2pseudocode
```

## Command Line Usage

### Basic Usage

```bash
# Convert a C file to pseudocode
c2pseudocode myfile.c

# Save to a file
c2pseudocode myfile.c -o output.txt

# Filter out system headers (recommended for files with includes)
c2pseudocode myfile.c --only-from-file

# Without preprocessing
c2pseudocode preprocessed.i --no-cpp
```

### Using as a Python Module

```bash
python3 -m c2pseudocode myfile.c
```

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
  IF n LESS_EQUAL 1 THEN
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
])

pseudocode = convert_c_file_to_pseudocode("myfile.c", cpp_args=cpp_args)
```

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

### Project Structure

```
c2pseudocode/
├── src/c2pseudocode/           # Main package
│   ├── __init__.py            # Public API
│   ├── __main__.py            # CLI entry point
│   ├── api.py                 # Core conversion functions
│   └── pseudocode_generator.py # AST visitor
├── tests/                     # Test suite
│   ├── fixtures/              # Test C files
│   │   ├── t_for_loops.c
│   │   ├── t_func_trivial.c
│   │   └── ...
│   └── test_conversion.py     # Pytest tests
├── tools/unifdef/             # unifdef C utility
├── pyproject.toml             # Package configuration
├── requirements.txt           # Dependencies
└── README.md                  # Documentation
```

## API Reference

### `convert_c_string_to_pseudocode(c_code, cpp_args=None)`

Convert C code string to pseudocode without preprocessing.

**Parameters:**
- `c_code` (str): C source code as a string
- `cpp_args` (list, optional): Preprocessor arguments for fake typedefs

**Returns:** str - Generated pseudocode

**Raises:** `pycparser.plyparser.ParseError` if C code has syntax errors

### `convert_c_file_to_pseudocode(filename, cpp_args=None, only_from_file=False)`

Convert a C file to pseudocode with preprocessing.

**Parameters:**
- `filename` (str): Path to C source file
- `cpp_args` (list, optional): Preprocessor arguments
- `only_from_file` (bool): Only convert code from the file, not headers

**Returns:** str - Generated pseudocode

**Raises:** 
- `RuntimeError` if preprocessing fails
- `pycparser.plyparser.ParseError` if C code has syntax errors

### `get_default_cpp_args()`

Get default C preprocessor arguments for GCC built-ins.

**Returns:** list - List of cpp argument strings

## Building unifdef

The project includes the `unifdef` utility in `tools/unifdef/`:

```bash
cd tools/unifdef
make
sudo make install  # Optional
```

See [tools/unifdef/README.md](tools/unifdef/README.md) for details.
