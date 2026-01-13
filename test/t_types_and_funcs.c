#include <stdint.h>
#include <stddef.h>

/* Global variables */
int global_int = 42;
const char *global_string = "hello";
double global_double = 3.14159;
static int module_static_int = 100;

/* Enum definitions */
enum Color {
    RED = 0,
    GREEN = 1,
    BLUE = 2
};

enum Status {
    SUCCESS,
    FAILURE,
    PENDING
};

/* Struct definitions */
struct Point {
    int x;
    int y;
};

struct Person {
    const char *name;
    int age;
    double height;
};

typedef struct {
    uint32_t id;
    enum Status status;
    struct Point location;
} Record;

/* Function declarations and definitions */
void simple_void_func(void);
int return_int(void);
char *return_pointer(void);
void take_int_return_void(int value);
int take_multiple_args(int a, double b, const char *c);
struct Point take_struct_return_struct(struct Point p);
void take_pointer(int *ptr);
void take_const_pointer(const int *ptr);
int *return_pointer_to_int(void);
void (*return_function_pointer(void))(void);
int take_variadic(int count, ...);
void take_function_pointer(int (*callback)(int));

/* Function implementations */
void simple_void_func(void) {
    int local_var = 5;
    const char *local_string = "local";
}

int return_int(void) {
    static int call_count = 0;
    call_count++;
    return call_count;
}

char *return_pointer(void) {
    return (char *)"test string";
}

void take_int_return_void(int value) {
    int result = value * 2;
}

int take_multiple_args(int a, double b, const char *c) {
    int len = 0;
    return a + (int)b + len;
}

struct Point take_struct_return_struct(struct Point p) {
    struct Point result = {p.x * 2, p.y * 2};
    return result;
}

void take_pointer(int *ptr) {
    if (ptr != NULL) {
        *ptr = 99;
    }
}

void take_const_pointer(const int *ptr) {
    int value = *ptr;
}

int *return_pointer_to_int(void) {
    static int value = 42;
    return &value;
}

void (*return_function_pointer(void))(void) {
    return simple_void_func;
}

int take_variadic(int count, ...) {
    return count;
}

void take_function_pointer(int (*callback)(int)) {
    int result = callback(10);
}
