#!/usr/bin/env python3
#------------------------------------------------------------------------------
# c2pseudocode command-line interface
#
# Uses pycparser and pseudocode_generator to translate C to
# Ada-like pseudocode.
#
# Based on code by Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
import sys
from .api import (
    convert_c_file_to_pseudocode,
    convert_preprocessed_file_to_pseudocode,
    get_default_cpp_args,
)

def translate_to_pseudocode(filename, output_file=None, use_cpp=True, only_from_file=False):
    """Use the pseudocode_generator module to emit Ada-like pseudocode from a C file.
    
    Args:
        filename: Input C file path
        output_file: Output file path (optional, defaults to stdout)
        use_cpp: Whether to use C preprocessor (default: True)
        only_from_file: If True, only generate pseudocode for code from the original file,
                       not from included headers (default: False)
    """
    try:
        if use_cpp:
            output = convert_c_file_to_pseudocode(
                filename, cpp_args=get_default_cpp_args(), only_from_file=only_from_file)
        else:
            output = convert_preprocessed_file_to_pseudocode(filename)
        
        if output_file:
            with open(output_file, 'w') as f:
                f.write(output)
            print(f"Pseudocode written to {output_file}", file=sys.stderr)
        else:
            print(output)
            
    except Exception as e:
        print(f"Error processing {filename}: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    """Main entry point for the command-line interface."""
    import argparse
    
    parser = argparse.ArgumentParser(
        prog='c2pseudocode',
        description='Convert C code to Ada-like pseudocode',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
  %(prog)s input.c                      # Output to stdout with all includes
  %(prog)s input.c -o output.txt        # Output to file
  %(prog)s input.i --no-cpp             # Input is already preprocessed
  %(prog)s input.c --only-from-file     # Only pseudocode from input file, not headers
        '''
    )
    
    parser.add_argument('input', help='Input C file')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    parser.add_argument('--no-cpp', action='store_true', 
                       help='Do not run the C preprocessor; use for input that is already preprocessed or has no #include/#define directives')
    parser.add_argument('--only-from-file', action='store_true',
                       help='Only generate pseudocode for code from the input file, not from #included headers')
    
    args = parser.parse_args()
    
    translate_to_pseudocode(args.input, args.output, use_cpp=not args.no_cpp, only_from_file=args.only_from_file)


if __name__ == "__main__":
    main()

