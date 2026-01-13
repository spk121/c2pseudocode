#include <stdio.h>

// Simple while loop
void test_simple_while(void) {
    int i = 0;
    while (i < 10) {
        printf("%d\n", i);
        i++;
    }
}

// While loop with break
void test_while_with_break(void) {
    int i = 0;
    while (1) {
        if (i == 5) break;
        printf("%d\n", i);
        i++;
    }
}

// While loop with continue
void test_while_with_continue(void) {
    int i = 0;
    while (i < 10) {
        i++;
        if (i % 2 == 0) continue;
        printf("%d\n", i);
    }
}

// Nested while loops
void test_nested_while(void) {
    int i = 0;
    while (i < 3) {
        int j = 0;
        while (j < 3) {
            printf("(%d, %d)\n", i, j);
            j++;
        }
        i++;
    }
}

// Simple do-while loop
void test_simple_do_while(void) {
    int i = 0;
    do {
        printf("%d\n", i);
        i++;
    } while (i < 10);
}

// Do-while with break
void test_do_while_with_break(void) {
    int i = 0;
    do {
        if (i == 5) break;
        printf("%d\n", i);
        i++;
    } while (1);
}

// Do-while with continue
void test_do_while_with_continue(void) {
    int i = 0;
    do {
        i++;
        if (i % 2 == 0) continue;
        printf("%d\n", i);
    } while (i < 10);
}

// Nested do-while loops
void test_nested_do_while(void) {
    int i = 0;
    do {
        int j = 0;
        do {
            printf("(%d, %d)\n", i, j);
            j++;
        } while (j < 3);
        i++;
    } while (i < 3);
}

// Mixed while and do-while
void test_mixed_loops(void) {
    int i = 0;
    while (i < 3) {
        int j = 0;
        do {
            printf("(%d, %d)\n", i, j);
            j++;
        } while (j < 2);
        i++;
    }
}

// While with complex condition
void test_while_complex_condition(void) {
    int x = 5, y = 10;
    while (x < 20 && y > 0) {
        printf("x=%d, y=%d\n", x, y);
        x += 2;
        y--;
    }
}

// Empty while loop
void test_empty_while(void) {
    int i = 0;
    while (i < 5) i++;
}

// Empty do-while loop
void test_empty_do_while(void) {
    int i = 0;
    do i++; while (i < 5);
}