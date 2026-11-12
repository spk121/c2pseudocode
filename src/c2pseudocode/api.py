"""Core API functions for C to pseudocode conversion."""

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

    The string is parsed as-is: it must not contain preprocessor directives
    (#include, #define, ...) and every type it uses must be declared in it.

    Args:
        c_code (str): C source code as a string
        cpp_args (list, optional): Ignored. The preprocessor is not run on
                                   strings; the parameter is kept only for
                                   backward compatibility.

    Returns:
        str: Generated pseudocode

    Raises:
        pycparser.c_parser.ParseError: If C code has syntax errors
    """
    ast = pycparser.CParser().parse(c_code, filename='<string>')
    return PseudocodeGenerator().visit(ast)


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
        FileNotFoundError: If the file does not exist
        RuntimeError: If preprocessing fails
        pycparser.c_parser.ParseError: If C code has syntax errors
    """
    if cpp_args is None:
        cpp_args = get_default_cpp_args()

    if not os.path.isfile(filename):
        raise FileNotFoundError(f"No such file: {filename}")

    try:
        ast = pycparser.parse_file(filename, use_cpp=True, cpp_args=cpp_args)
    except subprocess.CalledProcessError as e:
        raise RuntimeError(
            f"Preprocessing failed (cpp exited with status {e.returncode})") from e
    except OSError as e:
        raise RuntimeError(f"Could not run the C preprocessor 'cpp': {e}") from e

    if only_from_file:
        # The whole translation unit is parsed, so that types declared in
        # headers are known, and only then are the header's own declarations
        # dropped from the output.
        ast.ext = [ext for ext in ast.ext if _is_from_file(ext, filename)]

    # Generate pseudocode
    generator = PseudocodeGenerator()
    return generator.visit(ast)


def convert_preprocessed_file_to_pseudocode(filename):
    """Convert a C file to pseudocode without running the preprocessor.

    Use this for files that are already preprocessed (e.g. the output of
    ``cpp`` or ``gcc -E``) or that contain no preprocessor directives.

    Args:
        filename (str): Path to the C source file

    Returns:
        str: Generated pseudocode

    Raises:
        FileNotFoundError: If the file does not exist
        pycparser.c_parser.ParseError: If C code has syntax errors
    """
    if not os.path.isfile(filename):
        raise FileNotFoundError(f"No such file: {filename}")
    ast = pycparser.parse_file(filename, use_cpp=False)
    return PseudocodeGenerator().visit(ast)


def _is_from_file(node, filename):
    """Return True if an AST node was defined in the given source file."""
    coord = getattr(node, 'coord', None)
    if coord is None or not coord.file:
        return False
    return os.path.abspath(coord.file) == os.path.abspath(filename)
