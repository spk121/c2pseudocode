#include <stdlib.h>

int main(void) {
    /* Memory management */
    void *ptr = malloc(100);
    void *arr = calloc(10, sizeof(int));
    ptr = realloc(ptr, 200);
    free(ptr);

    /* Program control */
    int r = rand();
    srand(42);
    exit(0);

    /* String-to-number conversions */
    int i = atoi("42");
    long l = atol("100");
    double d = atof("3.14");
    long lv = strtol("0xFF", 0, 16);
    double dv = strtod("1.5e2", 0);

    return 0;
}
