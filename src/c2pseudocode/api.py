"""Core API functions for C to pseudocode conversion."""

import tempfile
import os
import subprocess
import pycparser
from .pseudocode_generator import PseudocodeGenerator


def get_default_cpp_args():
    """Get default C preprocessor arguments for GCC built-ins.
    
    Returns:
        list: List of cpp argument strings to handle GCC extensions.
    """
    return [
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
    ]


def convert_c_string_to_pseudocode(c_code, cpp_args=None):
    """Convert C code string to pseudocode without preprocessing.
    
    Args:
        c_code (str): C source code as a string
        cpp_args (list, optional): List of preprocessor arguments for fake typedefs.
                                   If None, uses get_default_cpp_args().
    
    Returns:
        str: Generated pseudocode
        
    Raises:
        pycparser.plyparser.ParseError: If C code has syntax errors
    """
    if cpp_args is None:
        cpp_args = get_default_cpp_args()
    
    # Write C code to a temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
        f.write(c_code)
        temp_file = f.name
    
    try:
        # Parse without preprocessing (code is already plain C)
        ast = pycparser.parse_file(temp_file, use_cpp=False)
        
        # Generate pseudocode
        generator = PseudocodeGenerator()
        return generator.visit(ast)
    finally:
        os.unlink(temp_file)


def convert_c_file_to_pseudocode(filename, cpp_args=None, only_from_file=False):
    """Convert a C file to pseudocode with preprocessing.
    
    Args:
        filename (str): Path to C source file
        cpp_args (list, optional): List of preprocessor arguments. 
                                   If None, uses get_default_cpp_args().
        only_from_file (bool): If True, only convert code from the specified file,
                              not from included headers. Default is False.
    
    Returns:
        str: Generated pseudocode
        
    Raises:
        RuntimeError: If preprocessing fails
        pycparser.plyparser.ParseError: If C code has syntax errors
    """
    if cpp_args is None:
        cpp_args = get_default_cpp_args()
    
    if only_from_file:
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
        # Normal parsing with cpp
        ast = pycparser.parse_file(filename, use_cpp=True, cpp_args=cpp_args)
    
    # Generate pseudocode
    generator = PseudocodeGenerator()
    return generator.visit(ast)


def _preprocess_and_filter(filename, cpp_args):
    """Preprocess a file and filter to only include lines from the original file.
    
    Uses cpp to preprocess, then parses the line markers to identify which
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
