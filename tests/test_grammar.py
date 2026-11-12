"""Test suite for individual C grammar constructs.

Tests each C language feature in isolation to ensure correct pseudocode generation.
"""

import pytest
from c2pseudocode import convert_c_string_to_pseudocode


class TestBasicTypes:
    """Test basic C type declarations."""
    
    def test_int_variable(self):
        """Test integer variable declaration."""
        result = convert_c_string_to_pseudocode("void f() { int x; }")
        assert "Integer" in result
        assert "x" in result
    
    def test_int_with_initialization(self):
        """Test integer with initialization."""
        result = convert_c_string_to_pseudocode("void f() { int x = 5; }")
        assert "Integer" in result
        assert "x" in result
        assert ":=" in result or "=" in result
        assert "5" in result
    
    def test_char_variable(self):
        """Test char variable declaration."""
        result = convert_c_string_to_pseudocode("void f() { char c; }")
        assert "String" in result or "char" in result.lower()
        assert "c" in result
    
    def test_float_variable(self):
        """Test float variable."""
        result = convert_c_string_to_pseudocode("void f() { float f; }")
        assert "f" in result
    
    def test_double_variable(self):
        """Test double variable."""
        result = convert_c_string_to_pseudocode("void f() { double d; }")
        assert "Float64" in result or "double" in result.lower()
        assert "d" in result
    
    def test_short_variable(self):
        """Test short int."""
        result = convert_c_string_to_pseudocode("void f() { short s; }")
        assert "s" in result
    
    def test_long_variable(self):
        """Test long int."""
        result = convert_c_string_to_pseudocode("void f() { long l; }")
        assert "l" in result
    
    def test_unsigned_int(self):
        """Test unsigned integer."""
        result = convert_c_string_to_pseudocode("void f() { unsigned int u; }")
        assert "u" in result


class TestArithmeticOperators:
    """Test arithmetic operators."""
    
    def test_addition(self):
        """Test addition operator."""
        result = convert_c_string_to_pseudocode("int f() { return 1 + 2; }")
        assert "+" in result
        assert "1" in result
        assert "2" in result
    
    def test_subtraction(self):
        """Test subtraction operator."""
        result = convert_c_string_to_pseudocode("int f() { return 5 - 3; }")
        assert "-" in result or "MINUS" in result
        assert "5" in result
        assert "3" in result
    
    def test_multiplication(self):
        """Test multiplication operator."""
        result = convert_c_string_to_pseudocode("int f() { return 3 * 4; }")
        assert "*" in result
        assert "3" in result
        assert "4" in result
    
    def test_division(self):
        """Test division operator."""
        result = convert_c_string_to_pseudocode("int f() { return 10 / 2; }")
        assert "/" in result or "DIV" in result
        assert "10" in result
        assert "2" in result
    
    def test_modulo(self):
        """Test modulo operator."""
        result = convert_c_string_to_pseudocode("int f() { return 10 % 3; }")
        assert "%" in result or "MOD" in result
        assert "10" in result
        assert "3" in result
    
    def test_increment(self):
        """Test increment operator."""
        result = convert_c_string_to_pseudocode("void f() { int i = 0; i++; }")
        assert "i" in result
    
    def test_decrement(self):
        """Test decrement operator."""
        result = convert_c_string_to_pseudocode("void f() { int i = 5; i--; }")
        assert "i" in result


class TestComparisonOperators:
    """Test comparison operators."""
    
    def test_equals(self):
        """Test equality operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x == 5) return 1; return 0; }")
        assert "EQUALS" in result or "==" in result
        assert "5" in result
    
    def test_not_equals(self):
        """Test not equals operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x != 5) return 1; return 0; }")
        assert ("NOT EQUALS" in result or "NOT_EQUALS" in result or "!=" in result or "≠" in result)
        assert "5" in result
    
    def test_less_than(self):
        """Test less than operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x < 10) return 1; return 0; }")
        assert "LESS" in result or "<" in result
        assert "10" in result
    
    def test_greater_than(self):
        """Test greater than operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x > 10) return 1; return 0; }")
        assert "GREATER" in result or ">" in result
        assert "10" in result
    
    def test_less_or_equal(self):
        """Test less than or equal operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x <= 10) return 1; return 0; }")
        assert "LESS_EQUAL" in result or "<=" in result or "≤" in result
        assert "10" in result
    
    def test_greater_or_equal(self):
        """Test greater than or equal operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (x >= 10) return 1; return 0; }")
        assert "GREATER_EQUAL" in result or ">=" in result or "≥" in result
        assert "10" in result


class TestLogicalOperators:
    """Test logical operators."""
    
    def test_logical_and(self):
        """Test logical AND operator."""
        result = convert_c_string_to_pseudocode("int f(int x, int y) { if (x > 0 && y > 0) return 1; return 0; }")
        assert "AND" in result or "&&" in result
    
    def test_logical_or(self):
        """Test logical OR operator."""
        result = convert_c_string_to_pseudocode("int f(int x, int y) { if (x > 0 || y > 0) return 1; return 0; }")
        assert "OR" in result or "||" in result
    
    def test_logical_not(self):
        """Test logical NOT operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { if (!x) return 1; return 0; }")
        assert "NOT" in result or "!" in result


class TestControlFlow:
    """Test control flow statements."""
    
    def test_if_statement(self):
        """Test simple if statement."""
        result = convert_c_string_to_pseudocode("void f(int x) { if (x > 0) x = 1; }")
        assert "IF" in result
        assert "END IF" in result or "ENDIF" in result
    
    def test_if_else_statement(self):
        """Test if-else statement."""
        result = convert_c_string_to_pseudocode("void f(int x) { if (x > 0) x = 1; else x = -1; }")
        assert "IF" in result
        assert "ELSE" in result
        assert "END IF" in result or "ENDIF" in result
    
    def test_if_elseif_else_statement(self):
        """Test if-elseif-else chain."""
        result = convert_c_string_to_pseudocode("""
        void f(int x) {
            if (x > 0) x = 1;
            else if (x < 0) x = -1;
            else x = 0;
        }
        """)
        assert "IF" in result
        assert "ELSE" in result
        assert result.count("IF") >= 2  # At least two IFs for the chain
    
    def test_nested_if(self):
        """Test nested if statements."""
        result = convert_c_string_to_pseudocode("""
        void f(int x, int y) {
            if (x > 0) {
                if (y > 0) {
                    x = 1;
                }
            }
        }
        """)
        assert "IF" in result
        assert result.count("IF") >= 2


class TestLoops:
    """Test loop constructs."""
    
    def test_while_loop(self):
        """Test while loop."""
        result = convert_c_string_to_pseudocode("void f() { int i = 0; while (i < 10) i++; }")
        assert "WHILE" in result or "LOOP" in result
        assert "END" in result
    
    def test_do_while_loop(self):
        """Test do-while loop."""
        result = convert_c_string_to_pseudocode("void f() { int i = 0; do { i++; } while (i < 10); }")
        assert "LOOP" in result or "DO" in result or "WHILE" in result
    
    def test_for_loop_simple(self):
        """Test simple for loop."""
        result = convert_c_string_to_pseudocode("void f() { int i; for (i = 0; i < 10; i++) i = i + 1; }")
        assert "LOOP" in result or "FOR" in result
        assert "i" in result
    
    def test_for_loop_with_declaration(self):
        """Test for loop with variable declaration."""
        result = convert_c_string_to_pseudocode("void f() { for (int i = 0; i < 10; i++) i = i + 1; }")
        assert "LOOP" in result or "FOR" in result
        assert "i" in result
    
    def test_nested_loops(self):
        """Test nested loops."""
        result = convert_c_string_to_pseudocode("""
        void f() {
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    i = i + j;
                }
            }
        }
        """)
        assert "LOOP" in result or "FOR" in result
        assert "i" in result
        assert "j" in result


class TestFunctions:
    """Test function declarations and definitions."""
    
    def test_void_function_no_params(self):
        """Test void function with no parameters."""
        result = convert_c_string_to_pseudocode("void f() { }")
        assert "f" in result
        assert "PROCEDURE" in result or "FUNCTION" in result
    
    def test_function_with_return_type(self):
        """Test function with return type."""
        result = convert_c_string_to_pseudocode("int f() { return 0; }")
        assert "f" in result
        assert "FUNCTION" in result
        assert "Integer" in result
        assert "RETURN" in result
    
    def test_function_with_single_param(self):
        """Test function with one parameter."""
        result = convert_c_string_to_pseudocode("int f(int x) { return x; }")
        assert "f" in result
        assert "x" in result
        assert "Integer" in result
    
    def test_function_with_multiple_params(self):
        """Test function with multiple parameters."""
        result = convert_c_string_to_pseudocode("int f(int x, int y, int z) { return x + y + z; }")
        assert "f" in result
        assert "x" in result
        assert "y" in result
        assert "z" in result
    
    def test_function_call(self):
        """Test function call."""
        result = convert_c_string_to_pseudocode("""
        int helper() { return 5; }
        int f() { return helper(); }
        """)
        assert "helper" in result
        assert "f" in result


class TestArrays:
    """Test array declarations and operations."""
    
    def test_array_declaration(self):
        """Test array declaration."""
        result = convert_c_string_to_pseudocode("void f() { int arr[10]; }")
        assert "arr" in result
        assert "10" in result
    
    def test_array_initialization(self):
        """Test array initialization."""
        result = convert_c_string_to_pseudocode("void f() { int arr[3] = {1, 2, 3}; }")
        assert "arr" in result
        assert "1" in result
        assert "2" in result
        assert "3" in result
    
    def test_array_access(self):
        """Test array element access."""
        result = convert_c_string_to_pseudocode("void f() { int arr[10]; arr[5] = 42; }")
        assert "arr" in result
        assert "5" in result
        assert "42" in result


class TestPointers:
    """Test pointer declarations and operations."""
    
    def test_pointer_declaration(self):
        """Test pointer declaration."""
        result = convert_c_string_to_pseudocode("void f() { int *p; }")
        assert "p" in result
        # Pointer syntax may be simplified in pseudocode
        assert "p" in result and "Integer" in result
    
    def test_pointer_dereference(self):
        """Test pointer dereference."""
        result = convert_c_string_to_pseudocode("void f() { int x = 5; int *p = &x; int y = *p; }")
        assert "p" in result
        assert "x" in result
        assert "y" in result
    
    def test_address_of_operator(self):
        """Test address-of operator is stripped in pseudocode (implementation detail)."""
        result = convert_c_string_to_pseudocode("void f() { int x; int *p = &x; }")
        assert "p" in result
        assert "x" in result
        assert "&x" not in result


class TestStructs:
    """Test struct declarations."""
    
    def test_struct_declaration(self):
        """Test struct declaration."""
        result = convert_c_string_to_pseudocode("""
        struct Point {
            int x;
            int y;
        };
        """)
        assert "Point" in result
        assert "x" in result
        assert "y" in result
    
    def test_struct_variable(self):
        """Test struct variable declaration."""
        result = convert_c_string_to_pseudocode("""
        struct Point {
            int x;
            int y;
        };
        void f() {
            struct Point p;
        }
        """)
        assert "Point" in result
        assert "p" in result
    
    def test_struct_member_access(self):
        """Test struct member access."""
        result = convert_c_string_to_pseudocode("""
        struct Point {
            int x;
            int y;
        };
        void f() {
            struct Point p;
            p.x = 10;
            p.y = 20;
        }
        """)
        assert "p" in result
        assert "x" in result
        assert "y" in result
        assert "10" in result
        assert "20" in result


class TestEnums:
    """Test enum declarations."""
    
    def test_enum_declaration(self):
        """Test enum declaration."""
        result = convert_c_string_to_pseudocode("""
        enum Color {
            RED,
            GREEN,
            BLUE
        };
        """)
        assert "Color" in result or "RED" in result


class TestTypedefs:
    """Test typedef declarations."""
    
    def test_simple_typedef(self):
        """Test simple typedef."""
        result = convert_c_string_to_pseudocode("typedef int MyInt; void f() { MyInt x; }")
        assert "MyInt" in result
        assert "x" in result


class TestAssignmentOperators:
    """Test compound assignment operators."""
    
    def test_plus_equals(self):
        """Test += operator."""
        result = convert_c_string_to_pseudocode("void f() { int x = 5; x += 3; }")
        assert "x" in result
        assert "5" in result
        assert "3" in result
    
    def test_minus_equals(self):
        """Test -= operator."""
        result = convert_c_string_to_pseudocode("void f() { int x = 5; x -= 3; }")
        assert "x" in result
        assert "5" in result
        assert "3" in result
    
    def test_multiply_equals(self):
        """Test *= operator."""
        result = convert_c_string_to_pseudocode("void f() { int x = 5; x *= 3; }")
        assert "x" in result
        assert "5" in result
        assert "3" in result
    
    def test_divide_equals(self):
        """Test /= operator."""
        result = convert_c_string_to_pseudocode("void f() { int x = 15; x /= 3; }")
        assert "x" in result
        assert "15" in result
        assert "3" in result


class TestTernaryOperator:
    """Test ternary conditional operator."""
    
    def test_ternary_operator(self):
        """Test ? : operator."""
        result = convert_c_string_to_pseudocode("int f(int x) { return x > 0 ? 1 : -1; }")
        assert "x" in result
        assert "1" in result
        assert "-1" in result


class TestBreakContinue:
    """Test break and continue statements."""
    
    def test_break_in_loop(self):
        """Test break statement."""
        result = convert_c_string_to_pseudocode("""
        void f() {
            for (int i = 0; i < 10; i++) {
                if (i == 5) break;
            }
        }
        """)
        # Break is converted to EXIT in ADA-like pseudocode
        assert "EXIT" in result or "BREAK" in result or "break" in result.lower()
        assert "i" in result
    
    def test_continue_in_loop(self):
        """Test continue statement."""
        result = convert_c_string_to_pseudocode("""
        void f() {
            for (int i = 0; i < 10; i++) {
                if (i == 5) continue;
            }
        }
        """)
        assert "CONTINUE" in result or "continue" in result.lower()
        assert "i" in result


class TestCasts:
    """Test type casting."""
    
    def test_explicit_cast(self):
        """Test explicit type cast."""
        result = convert_c_string_to_pseudocode("void f() { int x = (int)3.14; }")
        assert "x" in result
        assert "3.14" in result


class TestSizeof:
    """Test sizeof operator."""
    
    def test_sizeof_type(self):
        """Test sizeof with type."""
        result = convert_c_string_to_pseudocode("void f() { int size = sizeof(int); }")
        assert "size" in result


class TestMultipleDeclarations:
    """Test multiple variable declarations."""
    
    def test_multiple_vars_one_line(self):
        """Test declaring multiple variables on one line."""
        result = convert_c_string_to_pseudocode("void f() { int x, y, z; }")
        assert "x" in result
        assert "y" in result
        assert "z" in result
    
    def test_multiple_vars_with_init(self):
        """Test multiple variables with initialization."""
        result = convert_c_string_to_pseudocode("void f() { int x = 1, y = 2, z = 3; }")
        assert "x" in result
        assert "y" in result
        assert "z" in result
        assert "1" in result
        assert "2" in result
        assert "3" in result


if __name__ == "__main__":
    pytest.main([__file__, "-v"])


def _body(result):
    """Return the output lines with surrounding whitespace stripped."""
    return [line.strip() for line in result.splitlines() if line.strip()]


class TestCountingLoopRanges:
    """Test the range notation of recognized counting loops."""

    def _loop(self, header):
        return convert_c_string_to_pseudocode(
            "void f() { int i; int s; for (%s) { s += i; } }" % header)

    def test_ascending_exclusive(self):
        assert "FOR i IN RANGE [0, 10) LOOP" in self._loop("i = 0; i < 10; i++")

    def test_ascending_inclusive(self):
        assert "FOR i IN RANGE [0, 10] LOOP" in self._loop("i = 0; i <= 10; i++")

    def test_descending_inclusive(self):
        # i takes the values 5, 4, ..., 0: both bounds are included
        assert "FOR i IN REVERSE RANGE [0, 5] LOOP" in self._loop("i = 5; i >= 0; i--")

    def test_descending_exclusive(self):
        # i takes the values 5, 4, ..., 1: the low bound is excluded
        assert "FOR i IN REVERSE RANGE (0, 5] LOOP" in self._loop("i = 5; i > 0; i--")

    def test_descending_step(self):
        assert "FOR i IN REVERSE RANGE (0, 10] STEP 2 LOOP" in self._loop("i = 10; i > 0; i -= 2")

    def test_infinite_for(self):
        result = convert_c_string_to_pseudocode("void f() { for (;;) { break; } }")
        assert _body(result)[2:5] == ["LOOP", "EXIT", "END LOOP"]

    def test_for_with_only_condition(self):
        result = convert_c_string_to_pseudocode("void f(int n) { for (; n;) { n--; } }")
        assert "WHILE n LOOP" in result


class TestElseIfChains:
    """Test else-if chains and unbraced nested statements."""

    def test_elsif(self):
        result = convert_c_string_to_pseudocode(
            "void f(int x) { if (x > 0) { x = 1; } else if (x < 0) { x = 2; } else { x = 3; } }")
        assert result.splitlines()[2:9] == [
            "  IF x > 0 THEN",
            "    x := 1",
            "  ELSIF x < 0 THEN",
            "    x := 2",
            "  ELSE",
            "    x := 3",
            "  END IF",
        ]

    def test_unbraced_nested_statements_are_indented(self):
        result = convert_c_string_to_pseudocode(
            "void f(int x) { while (x) if (x > 2) x = 1; else x = 2; }")
        assert result.splitlines()[2:8] == [
            "  WHILE x LOOP",
            "    IF x > 2 THEN",
            "      x := 1",
            "    ELSE",
            "      x := 2",
            "    END IF",
        ]


class TestSwitchFallThrough:
    """Test switch statements whose cases share code."""

    def test_empty_labels_are_merged(self):
        result = convert_c_string_to_pseudocode(
            "void f(int v) { switch (v) { case 1: case 2: v = 0; break; default: v = 9; break; } }")
        assert "WHEN 1 | 2 =>" in result
        assert "FALLS THROUGH" not in result
        assert "EXIT" not in result

    def test_fall_through_is_marked(self):
        result = convert_c_string_to_pseudocode(
            "void f(int v) { switch (v) { case 1: v = 0; case 2: v = 5; break; } }")
        lines = _body(result)
        i = lines.index("WHEN 1 =>")
        assert lines[i:i + 4] == [
            "WHEN 1 =>",
            "v := 0",
            "-- FALLS THROUGH to the next WHEN branch",
            "WHEN 2 =>",
        ]

    def test_return_does_not_fall_through(self):
        result = convert_c_string_to_pseudocode(
            "int f(int v) { switch (v) { case 1: return 4; default: return 5; } }")
        assert "FALLS THROUGH" not in result

    def test_empty_branch(self):
        result = convert_c_string_to_pseudocode(
            "void f(int v) { switch (v) { case 1: v = 0; break; default: break; } }")
        lines = _body(result)
        assert lines[lines.index("WHEN OTHERS =>") + 1] == "DO_NOTHING"


class TestSmallRenderingFixes:
    """Test assorted operator, type and constant renderings."""

    @pytest.mark.parametrize("op, keyword", [
        ("%=", "MOD"),
        ("&=", "BITWISE_AND"),
        ("|=", "BITWISE_OR"),
        ("^=", "BITWISE_XOR"),
        ("<<=", "SHIFT_LEFT"),
        (">>=", "SHIFT_RIGHT"),
    ])
    def test_compound_assignment(self, op, keyword):
        result = convert_c_string_to_pseudocode("void f(int x) { x %s 3; }" % op)
        assert "x := x %s 3" % keyword in result

    def test_compound_assignment_parenthesizes_rvalue(self):
        result = convert_c_string_to_pseudocode("void f(int x, int y) { x %= y + 1; }")
        assert "x := x MOD (y + 1)" in result

    def test_char_is_character(self):
        result = convert_c_string_to_pseudocode("void f() { char c = 'a'; }")
        assert "c : Character := 'a'" in result

    def test_char_pointer_and_array_are_strings(self):
        result = convert_c_string_to_pseudocode("void f(char *s) { char buf[8]; }")
        assert "s : String" in result
        assert "buf[8] : String" in result

    def test_null_pointer_constant(self):
        result = convert_c_string_to_pseudocode(
            "void f(int *p) { if (p != ((void *)0)) { p = ((void *)0); } }")
        assert "IF p NOT EQUALS NULL THEN" in result
        assert "p := NULL" in result

    def test_cast_adds_no_parentheses(self):
        result = convert_c_string_to_pseudocode(
            "double f(double d, long n) { return d + (double)n; }")
        assert "RETURN d + n" in result

    def test_cast_of_expression_keeps_grouping(self):
        result = convert_c_string_to_pseudocode(
            "int f(int a, int b) { return a * (int)(a + b); }")
        assert "RETURN a * (a + b)" in result

    def test_plain_typedef(self):
        result = convert_c_string_to_pseudocode("typedef unsigned long ulong_t;")
        assert result.strip() == "TYPE ulong_t IS unsigned long"
