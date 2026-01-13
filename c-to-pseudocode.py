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
import subprocess
import tempfile
import os
import pseudocode_generator
import pycparser

def _preprocess_and_filter(filename, cpp_args):
    """ Preprocess a file and filter to only include lines from the original file.
    
    Uses gcc -E to preprocess, then parses the line markers to identify which
    lines come from the original file vs. system headers.
    
    Returns:
        Tuple of (preprocessed_code, filtered_code, line_mapping)
    """
    import re
    
    # Get the absolute path to normalize filenames
    original_file = os.path.abspath(filename)
    
    # Run preprocessor
    try:
        full_cpp_cmd = ['cpp'] + cpp_args + [filename]
        preprocessed = subprocess.check_output(
            full_cpp_cmd,
            stderr=subprocess.DEVNULL,
            universal_newlines=True
        )
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Preprocessing failed: {e}")
    
    lines = preprocessed.split('\n')
    filtered_lines = []
    current_file = None
    line_mapping = []  # Track which lines are from original file
    
    # Regex to match preprocessor line markers: # line "filename" [flags]
    line_marker_re = re.compile(r'^#\s*(\d+)\s+"([^"]+)"')
    
    for line in lines:
        match = line_marker_re.match(line)
        if match:
            # Update which file we're currently processing
            current_file = os.path.abspath(match.group(2))
            # Don't include the line marker itself in output
            continue
        
        # Only include lines from the original file
        if current_file == original_file:
            filtered_lines.append(line)
            line_mapping.append(True)
        else:
            line_mapping.append(False)
    
    filtered_code = '\n'.join(filtered_lines)
    return preprocessed, filtered_code, line_mapping

def translate_to_pseudocode(filename, output_file=None, use_cpp=True, only_from_file=False):
    """ Use the pseudocode_generator module to emit ADA-like pseudocode from a C file.
    
    Args:
        filename: Input C file path
        output_file: Output file path (optional, defaults to stdout)
        use_cpp: Whether to use C preprocessor (default: True)
        only_from_file: If True, only generate pseudocode for code from the original file,
                       not from included headers (default: False)
    """
    try:
        # Define GCC built-in types and keywords that pycparser doesn't know about
        cpp_args = [
            '-D__builtin_va_list=void*',
            '-D__builtin_va_arg(ap,type)=0',
            '-D__builtin_va_start(ap,last)=((ap)=(void*)0)',
            '-D__builtin_va_end(ap)=((ap)=(void*)0)',
            '-D__builtin_va_copy(dest,src)=((dest)=(src))',
            '-D__attribute__(x)=',
            '-D__restrict=restrict',
            '-D__asm__(x)=',
            '-D__extension__=',
            '-D__inline__=inline',
            '-D__inline=inline',
            # GCC floating-point types
            '-D_Float128=double',
            '-D_Float32=float',
            '-D_Float32x=double',
            '-D_Float64=double',
            '-D_Float64x=double',
            # GCC atomic and other extensions
            '-D_Atomic(type)=type',
            '-D_Noreturn=',
            '-D_Thread_local=',
        ] if use_cpp else []
        
        if use_cpp and only_from_file:
            # Preprocess and filter to only include code from original file
            _, filtered_code, _ = _preprocess_and_filter(filename, cpp_args)
            
            # Write filtered code to temp file and parse without cpp
            with tempfile.NamedTemporaryFile(mode='w', suffix='.i', delete=False) as f:
                f.write(filtered_code)
                temp_file = f.name
            
            try:
                ast = pycparser.parse_file(temp_file, use_cpp=False)
            finally:
                os.unlink(temp_file)
        else:
            # Normal parsing with or without cpp
            ast = pycparser.parse_file(filename, use_cpp=use_cpp, cpp_args=cpp_args if cpp_args else None)
        
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
  %(prog)s input.c                      # Output to stdout with all includes
  %(prog)s input.c -o output.txt        # Output to file
  %(prog)s input.c --no-cpp             # Don't use C preprocessor
  %(prog)s input.c --only-from-file     # Only pseudocode from input file, not headers
        '''
    )
    
    parser.add_argument('input', help='Input C file')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    parser.add_argument('--no-cpp', action='store_true', 
                       help='Do not use C preprocessor (required for files with system #includes like <stdio.h>)')
    parser.add_argument('--only-from-file', action='store_true',
                       help='Only generate pseudocode for code from the input file, not from #included headers')
    
    args = parser.parse_args()
    
    translate_to_pseudocode(args.input, args.output, use_cpp=not args.no_cpp, only_from_file=args.only_from_file)

