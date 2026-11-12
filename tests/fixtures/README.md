# Test Fixtures

This directory contains C source files used as test fixtures for the c2pseudocode test suite.

## Files

- `t_all_stdio_functions.c` - One call to each supported `stdio.h` function
- `t_comprehensive_stdio.c` - `stdio.h` functions combined in realistic code
- `t_for_loops.c` - Various for loop constructs
- `t_func_trivial.c` - Simple function declarations
- `t_header_types.c` - Code using types declared in system headers (`size_t`, `FILE`)
- `t_main_argc_argv.c` - Main function with command-line arguments
- `t_main_no_args.c` - Main function with no arguments
- `t_math_functions.c` - `math.h` function calls
- `t_std_includes.c` - File including the standard headers (for testing filtering)
- `t_stdio_functions.c` - Basic `stdio.h` function calls
- `t_stdlib_functions.c` - `stdlib.h` function calls
- `t_string_functions.c` - `string.h` function calls
- `t_types_and_funcs.c` - Various type declarations and functions
- `t_while.c` - While loop constructs

These files are used by the test suite in `tests/`, mainly `test_conversion.py`, to verify that C code is correctly converted to pseudocode.
