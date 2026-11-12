/* Uses types that are declared in system headers (size_t, FILE). */
#include <stdio.h>
#include <string.h>

size_t count_chars(FILE *fp, const char *prefix)
{
    size_t n = strlen(prefix);

    while (fgetc(fp) != EOF) {
        n++;
    }
    return n;
}
