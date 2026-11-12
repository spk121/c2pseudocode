#!/bin/bash
#
# Standalone wrapper script to run c2pseudocode without installing the package.
# (The pycparser dependency must still be installed: pip install pycparser)
# This allows you to run the tool directly from the repository.
#
# Usage:
#   ./c2pseudocode.sh input.c
#   ./c2pseudocode.sh input.c --only-from-file -o output.txt
#

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Run the c2pseudocode module straight from the source tree
export PYTHONPATH="$SCRIPT_DIR/src${PYTHONPATH:+:$PYTHONPATH}"
exec python3 -m c2pseudocode "$@"
