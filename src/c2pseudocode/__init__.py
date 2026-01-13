"""C to Pseudocode Converter

A tool to convert C source code to ADA-like pseudocode for documentation purposes.
"""

from .pseudocode_generator import PseudocodeGenerator
from .api import (
    convert_c_string_to_pseudocode,
    convert_c_file_to_pseudocode,
    get_default_cpp_args,
)

__version__ = "0.1.0"
__all__ = [
    "PseudocodeGenerator",
    "convert_c_string_to_pseudocode",
    "convert_c_file_to_pseudocode",
    "get_default_cpp_args",
]
