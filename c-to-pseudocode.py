#!/usr/bin/env python3
#------------------------------------------------------------------------------
# pycparser: c-to-pseudocode.py
#
# Example of using pycparser and pseudocode_generator to translate C to
# ADA-like pseudocode.
#
# Based on code by Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
import sys
import pseudocode_generator
import pycparser

def translate_to_pseudocode(filename, output_file=None, use_cpp=True):
    """ Use the pseudocode_generator module to emit ADA-like pseudocode from a C file.
    
    Args:
        filename: Input C file path
        output_file: Output file path (optional, defaults to stdout)
        use_cpp: Whether to use C preprocessor (default: True)
    """
    try:
        ast = pycparser.parse_file(filename, use_cpp=use_cpp)
        generator = pseudocode_generator.PseudocodeGenerator()
        output = generator.visit(ast)
        
        if output_file:
            with open(output_file, 'w') as f:
                f.write(output)
            print(f"Pseudocode written to {output_file}", file=sys.stderr)
        else:
            print(output)
            
    except Exception as e:
        print(f"Error processing {filename}: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Convert C code to ADA-like pseudocode',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
  %(prog)s input.c                    # Output to stdout
  %(prog)s input.c -o output.txt      # Output to file
  %(prog)s input.c --no-cpp           # Don't use C preprocessor
        '''
    )
    
    parser.add_argument('input', help='Input C file')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    parser.add_argument('--no-cpp', action='store_true', 
                       help='Do not use C preprocessor (useful for preprocessed files)')
    
    args = parser.parse_args()
    
    translate_to_pseudocode(args.input, args.output, use_cpp=not args.no_cpp)

