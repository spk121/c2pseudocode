"""Check that the examples in CONVERSION.md match the converter's real output.

Every C block introduced by "**C:**" that is followed by a block introduced by
"**Pseudocode:**" is converted and compared with the documented pseudocode.
"""

import re
from pathlib import Path

import pytest
from pycparser.c_parser import ParseError

from c2pseudocode import convert_c_string_to_pseudocode

CONVERSION_MD = Path(__file__).parent.parent / "CONVERSION.md"

EXAMPLE_RE = re.compile(
    r"\*\*C:\*\*\n```c\n(?P<c>.*?)```\n\n\*\*Pseudocode:\*\*\n```\n(?P<pseudo>.*?)```",
    re.DOTALL,
)


def _examples():
    text = CONVERSION_MD.read_text()
    return [(m.group("c"), m.group("pseudo")) for m in EXAMPLE_RE.finditer(text)]


def _convert(c_code):
    """Convert an example, which may be declarations or bare statements."""
    try:
        return convert_c_string_to_pseudocode(c_code)
    except ParseError:
        pass
    # Statements are only valid inside a function: wrap them, then unwrap
    # the body and remove its indentation.
    wrapped = convert_c_string_to_pseudocode("void example(void) {\n" + c_code + "}\n")
    lines = wrapped.splitlines()
    body = lines[lines.index("BEGIN") + 1:lines.index("END PROCEDURE")]
    return "\n".join(line[2:] for line in body)


def _normalize(text):
    """Ignore blank lines and trailing whitespace."""
    return [line.rstrip() for line in text.splitlines() if line.strip()]


def test_examples_are_found():
    assert len(_examples()) >= 15


@pytest.mark.parametrize("c_code, pseudocode", _examples())
def test_conversion_md_example(c_code, pseudocode):
    assert _normalize(_convert(c_code)) == _normalize(pseudocode)


def _table_rows(heading):
    """Return the cells of each body row of the table under a heading."""
    text = CONVERSION_MD.read_text()
    section = text.split(heading, 1)[1]
    rows = []
    for line in section.splitlines()[1:]:
        if line.startswith("|"):
            rows.append([cell.strip().strip("`") for cell in line.strip("|").split(" | ")])
        elif rows:
            break
    return rows[2:]  # skip the header and separator rows


def test_range_notation_table():
    rows = _table_rows("**Range notation.**")
    assert len(rows) == 4
    for c_loop, pseudocode, _values in rows:
        result = convert_c_string_to_pseudocode(
            "void example(void) { int i; %s { } }" % c_loop)
        assert pseudocode in result
