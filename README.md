# C to Pseudocode Converter

A tool to convert C source code to ADA-like pseudocode for documentation purposes.

## Quick Start

```bash
# Install dependencies
pip install pycparser

# Convert a C file (shows everything including system headers)
python3 c-to-pseudocode.py input.c

# Convert only the code from your file (recommended for most cases)
python3 c-to-pseudocode.py input.c --only-from-file -o output.txt
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

### Basic Conversion

```bash
# Convert a simple C file without includes
python3 c-to-pseudocode.py mycode.c

# Save output to a file
python3 c-to-pseudocode.py mycode.c -o pseudocode.txt
```

### Files with System Includes

When your C file includes system headers (like `<stdio.h>`), use `--only-from-file` to filter out the system library code and only show your own code:

```bash
# Only show pseudocode for code in mycode.c, not from <stdio.h>, <stdlib.h>, etc.
python3 c-to-pseudocode.py mycode.c --only-from-file

# Example: A file with #include <stdio.h> that would otherwise generate 
# hundreds of lines of system library code now shows only your functions
```

**Without `--only-from-file`:**
```bash
$ python3 c-to-pseudocode.py test/t_main_no_args.c | wc -l
239  # Includes all of stdio.h and its dependencies
```

**With `--only-from-file`:**
```bash
$ python3 c-to-pseudocode.py test/t_main_no_args.c --only-from-file
FUNCTION main() RETURN Integer IS
BEGIN
  RETURN 0
END FUNCTION
```

### Advanced Options

```bash
# Don't use C preprocessor (for already preprocessed files)
python3 c-to-pseudocode.py preprocessed.i --no-cpp

# Combine options
python3 c-to-pseudocode.py mycode.c --only-from-file -o output.txt
```

## Documentation

See [CONVERSION.md](CONVERSION.md) for complete conversion rules and examples.

## Files

- `c-to-pseudocode.py` - Main converter script
- `pseudocode_generator.py` - Core pseudocode generation logic
- `c2ada3.py` - Alternative regex-based converter (legacy)
- `unifdef.c` / `unifdef.exe` - Preprocessor conditional removal tool
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
