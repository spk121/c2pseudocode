"""Tests for stdlib, string, math function conversions and operator substitutions."""
import pytest
from c2pseudocode.api import convert_c_string_to_pseudocode


class TestStdlibFunctions:
    """Test stdlib.h function name conversions."""

    # Memory management
    def test_malloc(self):
        c_code = 'void test() { void *p = malloc(100); }'
        assert 'ALLOCATE(100)' in convert_c_string_to_pseudocode(c_code)

    def test_calloc(self):
        c_code = 'void test() { void *p = calloc(10, 4); }'
        assert 'ALLOCATE_ARRAY(10, 4)' in convert_c_string_to_pseudocode(c_code)

    def test_realloc(self):
        c_code = 'void test() { void *p; p = realloc(p, 200); }'
        assert 'REALLOCATE(p, 200)' in convert_c_string_to_pseudocode(c_code)

    def test_free(self):
        c_code = 'void test() { void *p; free(p); }'
        assert 'DEALLOCATE(p)' in convert_c_string_to_pseudocode(c_code)

    # Program control
    def test_exit(self):
        c_code = 'void test() { exit(0); }'
        assert 'EXIT_PROGRAM(0)' in convert_c_string_to_pseudocode(c_code)

    def test_abort(self):
        c_code = 'void test() { abort(); }'
        assert 'ABORT_PROGRAM()' in convert_c_string_to_pseudocode(c_code)

    def test_rand(self):
        c_code = 'void test() { int r = rand(); }'
        assert 'RANDOM_INTEGER()' in convert_c_string_to_pseudocode(c_code)

    def test_srand(self):
        c_code = 'void test() { srand(42); }'
        assert 'SET_RANDOM_SEED(42)' in convert_c_string_to_pseudocode(c_code)

    # String-to-number conversions
    def test_atoi(self):
        c_code = 'void test() { int i = atoi("42"); }'
        assert 'STRING_TO_INTEGER("42")' in convert_c_string_to_pseudocode(c_code)

    def test_atol(self):
        c_code = 'void test() { long l = atol("100"); }'
        assert 'STRING_TO_LONG("100")' in convert_c_string_to_pseudocode(c_code)

    def test_atof(self):
        c_code = 'void test() { double d = atof("3.14"); }'
        assert 'STRING_TO_FLOAT("3.14")' in convert_c_string_to_pseudocode(c_code)

    def test_strtol(self):
        c_code = 'void test() { long v = strtol("FF", 0, 16); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PARSE_INTEGER("FF", 16)' in result

    def test_strtod(self):
        c_code = 'void test() { double v = strtod("1.5e2", 0); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PARSE_FLOAT("1.5e2")' in result


class TestStringFunctions:
    """Test string.h function name conversions."""

    def test_strlen(self):
        c_code = 'void test() { char *s = "hi"; int n = strlen(s); }'
        assert 'STRING_LENGTH(s)' in convert_c_string_to_pseudocode(c_code)

    def test_strcpy(self):
        c_code = 'void test() { char dst[10]; char *src = "hi"; strcpy(dst, src); }'
        assert 'COPY_STRING(dst, src)' in convert_c_string_to_pseudocode(c_code)

    def test_strncpy(self):
        c_code = 'void test() { char dst[10]; char *src = "hi"; strncpy(dst, src, 5); }'
        assert 'COPY_STRING(dst, src, 5)' in convert_c_string_to_pseudocode(c_code)

    def test_strcat(self):
        c_code = 'void test() { char dst[10]; char *src = "hi"; strcat(dst, src); }'
        assert 'APPEND_STRING(dst, src)' in convert_c_string_to_pseudocode(c_code)

    def test_strncat(self):
        c_code = 'void test() { char dst[10]; char *src = "hi"; strncat(dst, src, 3); }'
        assert 'APPEND_STRING(dst, src, 3)' in convert_c_string_to_pseudocode(c_code)

    def test_strcmp(self):
        c_code = 'void test() { int r = strcmp("a", "b"); }'
        assert 'COMPARE_STRINGS("a", "b")' in convert_c_string_to_pseudocode(c_code)

    def test_strncmp(self):
        c_code = 'void test() { int r = strncmp("abc", "abd", 2); }'
        assert 'COMPARE_STRINGS("abc", "abd", 2)' in convert_c_string_to_pseudocode(c_code)

    def test_strchr(self):
        c_code = "void test() { char *s = \"hello\"; char *p = strchr(s, 'l'); }"
        assert 'FIND_CHAR' in convert_c_string_to_pseudocode(c_code)

    def test_strrchr(self):
        c_code = "void test() { char *s = \"hello\"; char *p = strrchr(s, 'l'); }"
        assert 'FIND_LAST_CHAR' in convert_c_string_to_pseudocode(c_code)

    def test_strstr(self):
        c_code = 'void test() { char *h = "hello world"; char *p = strstr(h, "world"); }'
        assert 'FIND_STRING(h, "world")' in convert_c_string_to_pseudocode(c_code)

    def test_memcpy(self):
        c_code = 'void test() { char a[10]; char b[10]; memcpy(a, b, 10); }'
        assert 'COPY_MEMORY(a, b, 10)' in convert_c_string_to_pseudocode(c_code)

    def test_memmove(self):
        c_code = 'void test() { char a[10]; char b[10]; memmove(a, b, 10); }'
        assert 'MOVE_MEMORY(a, b, 10)' in convert_c_string_to_pseudocode(c_code)

    def test_memset(self):
        c_code = 'void test() { char a[10]; memset(a, 0, 10); }'
        assert 'FILL_MEMORY(a, 0, 10)' in convert_c_string_to_pseudocode(c_code)

    def test_memcmp(self):
        c_code = 'void test() { char a[10]; char b[10]; int r = memcmp(a, b, 10); }'
        assert 'COMPARE_MEMORY(a, b, 10)' in convert_c_string_to_pseudocode(c_code)


class TestMathFunctions:
    """Test math.h function name conversions."""

    def test_sqrt(self):
        c_code = 'void test() { double r = sqrt(4.0); }'
        assert 'SQUARE_ROOT(4.0)' in convert_c_string_to_pseudocode(c_code)

    def test_fabs(self):
        c_code = 'void test() { double r = fabs(-1.0); }'
        assert 'ABSOLUTE_VALUE(-1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_abs(self):
        c_code = 'void test() { int r = abs(-3); }'
        assert 'ABSOLUTE_VALUE(-3)' in convert_c_string_to_pseudocode(c_code)

    def test_pow(self):
        c_code = 'void test() { double r = pow(2.0, 3.0); }'
        assert 'POWER(2.0, 3.0)' in convert_c_string_to_pseudocode(c_code)

    def test_exp(self):
        c_code = 'void test() { double r = exp(1.0); }'
        assert 'EXPONENTIAL(1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_log(self):
        c_code = 'void test() { double r = log(2.0); }'
        assert 'NATURAL_LOG(2.0)' in convert_c_string_to_pseudocode(c_code)

    def test_log2(self):
        c_code = 'void test() { double r = log2(4.0); }'
        assert 'LOG2(4.0)' in convert_c_string_to_pseudocode(c_code)

    def test_log10(self):
        c_code = 'void test() { double r = log10(100.0); }'
        assert 'LOG10(100.0)' in convert_c_string_to_pseudocode(c_code)

    def test_ceil(self):
        c_code = 'void test() { double r = ceil(1.3); }'
        assert 'CEILING(1.3)' in convert_c_string_to_pseudocode(c_code)

    def test_floor(self):
        c_code = 'void test() { double r = floor(1.7); }'
        assert 'FLOOR(1.7)' in convert_c_string_to_pseudocode(c_code)

    def test_round(self):
        c_code = 'void test() { double r = round(1.5); }'
        assert 'ROUND(1.5)' in convert_c_string_to_pseudocode(c_code)

    def test_sin(self):
        c_code = 'void test() { double r = sin(1.0); }'
        assert 'SINE(1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_cos(self):
        c_code = 'void test() { double r = cos(1.0); }'
        assert 'COSINE(1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_tan(self):
        c_code = 'void test() { double r = tan(1.0); }'
        assert 'TANGENT(1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_asin(self):
        c_code = 'void test() { double r = asin(0.5); }'
        assert 'ARC_SINE(0.5)' in convert_c_string_to_pseudocode(c_code)

    def test_acos(self):
        c_code = 'void test() { double r = acos(0.5); }'
        assert 'ARC_COSINE(0.5)' in convert_c_string_to_pseudocode(c_code)

    def test_atan(self):
        c_code = 'void test() { double r = atan(1.0); }'
        assert 'ARC_TANGENT(1.0)' in convert_c_string_to_pseudocode(c_code)

    def test_atan2(self):
        c_code = 'void test() { double r = atan2(1.0, 2.0); }'
        assert 'ARC_TANGENT2(1.0, 2.0)' in convert_c_string_to_pseudocode(c_code)


class TestBinaryOperatorSubstitutions:
    """Test bitwise and modulo binary operator substitutions."""

    def test_bitwise_and(self):
        c_code = 'void test() { int r = 5 & 3; }'
        assert 'BITWISE_AND' in convert_c_string_to_pseudocode(c_code)

    def test_bitwise_or(self):
        c_code = 'void test() { int r = 5 | 3; }'
        assert 'BITWISE_OR' in convert_c_string_to_pseudocode(c_code)

    def test_bitwise_xor(self):
        c_code = 'void test() { int r = 5 ^ 3; }'
        assert 'BITWISE_XOR' in convert_c_string_to_pseudocode(c_code)

    def test_shift_right(self):
        c_code = 'void test() { int r = 8 >> 2; }'
        assert 'SHIFT_RIGHT' in convert_c_string_to_pseudocode(c_code)

    def test_shift_left(self):
        c_code = 'void test() { int r = 1 << 3; }'
        assert 'SHIFT_LEFT' in convert_c_string_to_pseudocode(c_code)

    def test_modulo(self):
        c_code = 'void test() { int r = 10 % 3; }'
        assert 'MOD' in convert_c_string_to_pseudocode(c_code)


class TestUnaryOperatorSubstitutions:
    """Test unary operator substitutions."""

    def test_bitwise_not(self):
        c_code = 'void test() { int r = ~5; }'
        assert 'BITWISE_NOT' in convert_c_string_to_pseudocode(c_code)

    def test_sizeof(self):
        c_code = 'void test() { int n = sizeof(int); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'SIZE_OF' in result
        assert 'sizeof' not in result

    def test_address_of_stripped(self):
        c_code = 'void test() { int x; int *p = &x; }'
        result = convert_c_string_to_pseudocode(c_code)
        # address-of should be stripped; raw & should not appear as an operator
        assert '&x' not in result

    def test_dereference_stripped(self):
        c_code = 'void test() { int x = 5; int *p = &x; int v = *p; }'
        result = convert_c_string_to_pseudocode(c_code)
        # dereference should be stripped; raw *p should not appear
        assert '*p' not in result
