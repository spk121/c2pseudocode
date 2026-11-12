"""Test suite for c2pseudocode conversion."""

import pytest
from pathlib import Path
from c2pseudocode import (
    convert_c_string_to_pseudocode,
    convert_c_file_to_pseudocode,
    get_default_cpp_args,
)


# Path to test fixtures
FIXTURES_DIR = Path(__file__).parent / "fixtures"


class TestStringConversion:
    """Test converting C code strings to pseudocode."""
    
    def test_simple_function(self):
        """Test converting a simple function."""
        c_code = """
        int add(int a, int b) {
            return a + b;
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "FUNCTION add" in result
        assert "Integer" in result
        assert "RETURN" in result
    
    def test_function_with_void(self):
        """Test converting a void function."""
        c_code = """
        void func(void) {
            int a = 10;
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "PROCEDURE func" in result or "FUNCTION func" in result
        assert "Integer" in result
    
    def test_main_function(self):
        """Test converting a main function."""
        c_code = """
        int main() {
            return 0;
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "main" in result
        assert "RETURN" in result
    
    def test_for_loop(self):
        """Test converting a for loop."""
        c_code = """
        void test(void) {
            int i;
            for (i = 0; i < 10; i++) {
                i = i + 1;
            }
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "LOOP" in result or "FOR" in result
    
    def test_while_loop(self):
        """Test converting a while loop."""
        c_code = """
        void test(void) {
            int i = 0;
            while (i < 10) {
                i = i + 1;
            }
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "WHILE" in result or "LOOP" in result
    
    def test_if_statement(self):
        """Test converting an if statement."""
        c_code = """
        void test(int x) {
            if (x > 0) {
                x = x + 1;
            }
        }
        """
        result = convert_c_string_to_pseudocode(c_code)
        
        assert "IF" in result


class TestFileConversion:
    """Test converting C files to pseudocode."""
    
    def test_trivial_function(self):
        """Test converting trivial function file."""
        fixture_file = FIXTURES_DIR / "t_func_trivial.c"
        result = convert_c_file_to_pseudocode(str(fixture_file))
        
        assert "func" in result
        assert "Integer" in result
    
    def test_main_no_args(self):
        """Test converting main with no args."""
        fixture_file = FIXTURES_DIR / "t_main_no_args.c"
        result = convert_c_file_to_pseudocode(str(fixture_file), only_from_file=True)
        
        assert "main" in result
        assert "RETURN" in result
        # Should not include stdio.h content when using only_from_file
    
    def test_for_loops(self):
        """Test converting for loops."""
        fixture_file = FIXTURES_DIR / "t_for_loops.c"
        result = convert_c_file_to_pseudocode(str(fixture_file))
        
        assert "test1" in result
        assert "LOOP" in result or "FOR" in result
    
    def test_while_loop(self):
        """Test converting while loop."""
        fixture_file = FIXTURES_DIR / "t_while.c"
        if fixture_file.exists():
            result = convert_c_file_to_pseudocode(str(fixture_file))
            assert "WHILE" in result or "LOOP" in result
    
    def test_types_and_funcs(self):
        """Test converting types and functions."""
        fixture_file = FIXTURES_DIR / "t_types_and_funcs.c"
        if fixture_file.exists():
            result = convert_c_file_to_pseudocode(str(fixture_file))
            assert result  # Just ensure it doesn't crash


class TestCustomCppArgs:
    """Test using custom cpp_args."""
    
    def test_custom_cpp_args(self):
        """Test providing custom cpp arguments."""
        c_code = """
        int main() {
            return 0;
        }
        """
        custom_args = get_default_cpp_args()
        custom_args.append("-DTEST_MACRO=1")
        
        result = convert_c_string_to_pseudocode(c_code, cpp_args=custom_args)
        assert "main" in result
    
    def test_empty_cpp_args(self):
        """Test with empty cpp_args list."""
        c_code = """
        int add(int a, int b) {
            return a + b;
        }
        """
        result = convert_c_string_to_pseudocode(c_code, cpp_args=[])
        assert "add" in result


class TestErrorHandling:
    """Test error handling."""
    
    def test_invalid_c_code(self):
        """Test that invalid C code raises an error."""
        c_code = "this is not valid C code {"
        
        with pytest.raises(Exception):
            convert_c_string_to_pseudocode(c_code)
    
    def test_nonexistent_file(self):
        """Test that nonexistent file raises an error."""
        with pytest.raises(Exception):
            convert_c_file_to_pseudocode("/nonexistent/file.c")


class TestOnlyFromFile:
    """Test the only_from_file functionality."""
    
    def test_with_includes(self):
        """Test that only_from_file excludes system headers."""
        fixture_file = FIXTURES_DIR / "t_std_includes.c"
        if fixture_file.exists():
            # With only_from_file=True, should be much shorter
            result_filtered = convert_c_file_to_pseudocode(
                str(fixture_file), 
                only_from_file=True
            )
            
            # Without only_from_file, includes system headers
            result_full = convert_c_file_to_pseudocode(
                str(fixture_file), 
                only_from_file=False
            )
            
            # Filtered version should be shorter or equal
            assert len(result_filtered) <= len(result_full)

    def test_types_from_headers(self):
        """Code using header-declared types (size_t, FILE) must still parse."""
        result = convert_c_file_to_pseudocode(
            str(FIXTURES_DIR / "t_header_types.c"), only_from_file=True)
        assert result.startswith(
            "FUNCTION count_chars(fp : FILE, prefix : String) RETURN size_t IS")
        assert "n : size_t := STRING_LENGTH(prefix)" in result
        # Nothing declared by the headers themselves is emitted
        assert result.count("FUNCTION") == 2  # FUNCTION ... END FUNCTION
        assert "TYPE" not in result

    def test_relative_path(self, monkeypatch):
        """Filtering must work when the file is given by a relative path."""
        monkeypatch.chdir(FIXTURES_DIR)
        result = convert_c_file_to_pseudocode("t_main_no_args.c", only_from_file=True)
        assert "FUNCTION main() RETURN Integer IS" in result


class TestCommandLine:
    """Test the command-line interface."""

    def _run(self, *args):
        import subprocess
        import sys
        return subprocess.run(
            [sys.executable, "-m", "c2pseudocode", *args],
            capture_output=True, text=True)

    def test_missing_file_reports_short_error(self):
        proc = self._run("/nonexistent/file.c")
        assert proc.returncode == 1
        assert "No such file" in proc.stderr
        assert "-D__builtin_va_list" not in proc.stderr

    def test_help_uses_program_name(self):
        proc = self._run("--help")
        assert proc.stdout.startswith("usage: c2pseudocode ")

    def test_no_cpp_does_not_preprocess(self, tmp_path):
        source = tmp_path / "plain.c"
        source.write_text("int add(int a, int b) { return a + b; }\n")
        proc = self._run(str(source), "--no-cpp")
        assert proc.returncode == 0
        assert "FUNCTION add(a : Integer, b : Integer) RETURN Integer IS" in proc.stdout

        # A directive proves the preprocessor was not run
        source.write_text("#define N 1\nint n = N;\n")
        assert self._run(str(source), "--no-cpp").returncode == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
