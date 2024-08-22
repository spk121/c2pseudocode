#------------------------------------------------------------------------------
# pycparser: c-to-c.py
#
# Example of using pycparser.c_generator, serving as a simplistic translator
# from C to AST and back to C.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
import sys
import pseudocode_generator
import pycparser

# This is not required if you've installed pycparser into
# your site-packages/ with setup.py
sys.path.extend(['.', '..'])

def translate_to_pseudocode(filename):
    """ Use the pseudocode_generator module to emit a parsed AST.
    """
    ast = pycparser.parse_file(filename)
    generator = pseudocode_generator.PseudocodeGenerator()
    print(generator.visit(ast), file=open('output.txt', 'w'))


if __name__ == "__main__":
    if len(sys.argv) > 1:
        #translate_to_pseudocode(sys.argv[1])
        translate_to_pseudocode('examples/circleload1.c')
    else:
        print("Please provide a filename as argument")
