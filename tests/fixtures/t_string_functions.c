#include <string.h>

int main(void) {
    char src[64] = "hello";
    char dst[64];
    char haystack[64] = "hello world";

    /* String operations */
    int len = strlen(src);
    strcpy(dst, src);
    strncpy(dst, src, 10);
    strcat(dst, src);
    strncat(dst, src, 5);
    int cmp = strcmp(src, dst);
    int ncmp = strncmp(src, dst, 3);
    char *cp = strchr(src, 'l');
    char *rcp = strrchr(src, 'l');
    char *sp = strstr(haystack, "world");

    /* Raw memory operations */
    memcpy(dst, src, 10);
    memmove(dst, src, 10);
    memset(dst, 0, 64);
    int mc = memcmp(src, dst, 10);

    return 0;
}
