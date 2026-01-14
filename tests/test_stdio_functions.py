"""Tests for stdio function conversions"""
import pytest
from c2pseudocode.api import convert_c_string_to_pseudocode


class TestStdioFunctions:
    """Test conversions for stdio functions like printf, fprintf, fopen, etc."""
    
    def test_printf_simple(self):
        """Test simple printf without format specifiers"""
        c_code = 'void test() { printf("Hello world"); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PRINT("Hello world")' in result
    
    def test_printf_with_single_arg(self):
        """Test printf with one format specifier"""
        c_code = '''
        void test() { 
            int i = 42;
            printf("counter %d", i); 
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PRINT("counter {i}")' in result
    
    def test_printf_with_multiple_args(self):
        """Test printf with multiple format specifiers"""
        c_code = '''
        void test() { 
            int i = 42;
            char *s = "test";
            printf("Value: %d, String: %s", i, s); 
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PRINT("Value: {i}, String: {s}")' in result
    
    def test_fprintf(self):
        """Test fprintf conversion"""
        c_code = '''
        void test() { 
            FILE *fp;
            int i = 42;
            fprintf(fp, "counter %d", i); 
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PRINT(fp, "counter {i}")' in result
    
    def test_fopen(self):
        """Test fopen conversion"""
        c_code = '''
        typedef void FILE;
        void test() { FILE *fp = fopen("file.txt", "rb"); }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'OPEN_FILE("file.txt", "rb")' in result
    
    def test_fclose(self):
        """Test fclose conversion"""
        c_code = 'void test() { FILE *fp; fclose(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'CLOSE_FILE(fp)' in result
    
    def test_fseek(self):
        """Test fseek conversion"""
        c_code = 'void test() { FILE *fp; fseek(fp, 0, SEEK_SET); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'SET_FILE_POSITION(fp, 0, START)' in result
    
    def test_ftell(self):
        """Test ftell conversion"""
        c_code = 'void test() { FILE *fp; long pos = ftell(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'GET_FILE_POSITION(fp)' in result
    
    def test_fputs(self):
        """Test fputs conversion"""
        c_code = 'void test() { FILE *fp; fputs("test", fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'WRITE_LINE("test", fp)' in result
    
    def test_fgets(self):
        """Test fgets conversion"""
        c_code = 'void test() { FILE *fp; char buf[100]; fgets(buf, 100, fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ_LINE(buf, fp)' in result
    
    def test_fread(self):
        """Test fread conversion"""
        c_code = 'void test() { FILE *fp; char buf[100]; fread(buf, 1, 100, fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ(buf, 1, 100, fp)' in result
    
    def test_fwrite(self):
        """Test fwrite conversion"""
        c_code = 'void test() { FILE *fp; char buf[100]; fwrite(buf, 1, 100, fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'WRITE(buf, 1, 100, fp)' in result
    
    def test_sprintf(self):
        """Test sprintf conversion"""
        c_code = '''
        void test() {
            char buf[100];
            int i = 42;
            sprintf(buf, "Value: %d", i);
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'FORMAT_STRING(buf, "Value: {i}")' in result
    
    def test_snprintf(self):
        """Test snprintf conversion"""
        c_code = '''
        void test() {
            char buf[100];
            int i = 42;
            snprintf(buf, 100, "Value: %d", i);
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'FORMAT_STRING(buf, "Value: {i}")' in result
    
    def test_scanf(self):
        """Test scanf conversion"""
        c_code = '''
        void test() {
            int i;
            scanf("%d", &i);
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ_INPUT("{i}")' in result
    
    def test_fscanf(self):
        """Test fscanf conversion"""
        c_code = '''
        void test() {
            FILE *fp;
            int i;
            fscanf(fp, "%d", &i);
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ_FROM_FILE(fp, "{i}")' in result
    
    def test_sscanf(self):
        """Test sscanf conversion"""
        c_code = '''
        void test() {
            char *str = "42";
            int i;
            sscanf(str, "%d", &i);
        }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'PARSE_STRING(str, "{i}")' in result
    
    def test_fgetc(self):
        """Test fgetc conversion"""
        c_code = 'void test() { FILE *fp; char c = fgetc(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ_CHAR(fp)' in result
    
    def test_fputc(self):
        """Test fputc conversion"""
        c_code = 'void test() { FILE *fp; fputc(\'A\', fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'WRITE_CHAR(\'A\', fp)' in result
    
    def test_getchar(self):
        """Test getchar conversion"""
        c_code = 'void test() { char c = getchar(); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'READ_CHAR()' in result
    
    def test_putchar(self):
        """Test putchar conversion"""
        c_code = 'void test() { putchar(\'A\'); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'WRITE_CHAR(\'A\')' in result
    
    def test_feof(self):
        """Test feof conversion"""
        c_code = 'void test() { FILE *fp; int x = feof(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'AT_END(fp)' in result
    
    def test_ferror(self):
        """Test ferror conversion"""
        c_code = 'void test() { FILE *fp; int x = ferror(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'HAS_ERROR(fp)' in result
    
    def test_clearerr(self):
        """Test clearerr conversion"""
        c_code = 'void test() { FILE *fp; clearerr(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'CLEAR_ERROR(fp)' in result
    
    def test_rewind(self):
        """Test rewind conversion"""
        c_code = 'void test() { FILE *fp; rewind(fp); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'REWIND(fp)' in result
    
    def test_remove(self):
        """Test remove conversion"""
        c_code = 'void test() { remove("file.txt"); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'DELETE_FILE("file.txt")' in result
    
    def test_rename(self):
        """Test rename conversion"""
        c_code = 'void test() { rename("old.txt", "new.txt"); }'
        result = convert_c_string_to_pseudocode(c_code)
        assert 'RENAME_FILE("old.txt", "new.txt")' in result
    
    def test_tmpfile(self):
        """Test tmpfile conversion"""
        c_code = '''
        typedef void FILE;
        void test() { FILE *fp = tmpfile(); }
        '''
        result = convert_c_string_to_pseudocode(c_code)
        assert 'CREATE_TEMP_FILE()' in result
