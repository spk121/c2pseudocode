# Test Fixtures

This directory contains C source files used as test fixtures for the c2pseudocode test suite.

## Files

- `t_for_loops.c` - Various for loop constructs
- `t_func_trivial.c` - Simple function declarations
- `t_main_argc_argv.c` - Main function with command-line arguments
- `t_main_no_args.c` - Main function with no arguments
- `t_std_includes.c` - File with system includes (for testing filtering)
- `t_types_and_funcs.c` - Various type declarations and functions
- `t_while.c` - While loop constructs

These files are used by the test suite in `../test_conversion.py` to verify that C code is correctly converted to pseudocode.
