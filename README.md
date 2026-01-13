# C to Pseudocode Converter

A tool to convert C source code to ADA-like pseudocode for documentation purposes.

## Quick Start

```bash
# Install dependencies
pip install pycparser

# Convert a C file
python3 c-to-pseudocode.py input.c -o output.txt
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
- **Operator clarity** - C operators converted to explicit keywords (== → =, && → AND)
- **Simplified control flow** - Uses END IF, END LOOP, etc. for clarity
- **Documentation-ready** - Output suitable for Software Design Documents

## Documentation

See [CONVERSION.md](CONVERSION.md) for complete conversion rules and examples.

## Files

- `c-to-pseudocode.py` - Main converter script
- `pseudocode_generator.py` - Core pseudocode generation logic
- `c2ada3.py` - Alternative regex-based converter (legacy)
- `unifdef.c` / `unifdef.exe` - Preprocessor conditional removal tool
- `CONVERSION.md` - Complete documentation of conversion rules

## Workflow

1. (Optional) Use `unifdef` to remove debug code and conditional compilation
2. Run `c-to-pseudocode.py` to generate pseudocode
3. Include the pseudocode in your documentation

Example with unifdef:

```bash
unifdef -DDEBUG=0 input.c | python3 c-to-pseudocode.py --no-cpp - > output.txt
```

## License

BSD License
