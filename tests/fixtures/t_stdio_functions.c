#include <stdio.h>

int main(void) {
    int i = 42;
    FILE *fp;
    
    /* printf examples */
    printf("Hello world\n");
    printf("counter %d\n", i);
    printf("Multiple values: %d %s\n", i, "test");
    
    /* fprintf examples */
    fp = fopen("output.txt", "wb");
    fprintf(fp, "hello");
    fprintf(fp, "counter %d\n", i);
    
    /* File operations */
    fclose(fp);
    
    /* Other stream functions */
    fseek(fp, 0, SEEK_SET);
    ftell(fp);
    fputs("test string", fp);
    fgets(buffer, 100, fp);
    fread(buffer, 1, 100, fp);
    fwrite(buffer, 1, 100, fp);
    
    return 0;
}
