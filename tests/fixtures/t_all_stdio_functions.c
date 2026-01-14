#include <stdio.h>

int main(void) {
    char buffer[256];
    char formatted_str[100];
    int value;
    FILE *fp;
    
    /* Output functions */
    printf("Simple message\n");
    printf("Number: %d, String: %s\n", 42, "hello");
    
    sprintf(formatted_str, "Value is %d", 100);
    snprintf(buffer, sizeof(buffer), "Limited: %d chars", 256);
    
    fp = fopen("data.txt", "r");
    fprintf(fp, "File message: %d\n", 99);
    fputs("Line text\n", fp);
    fputc('A', fp);
    
    /* Input functions */
    scanf("%d", &value);
    fscanf(fp, "%d", &value);
    sscanf("42", "%d", &value);
    
    fgets(buffer, sizeof(buffer), fp);
    char c = fgetc(fp);
    char ch = getchar();
    
    /* File position and status */
    fseek(fp, 0, SEEK_SET);
    long pos = ftell(fp);
    
    if (feof(fp)) {
        printf("End of file\n");
    }
    
    if (ferror(fp)) {
        printf("Error occurred\n");
    }
    
    clearerr(fp);
    rewind(fp);
    
    /* File operations */
    fread(buffer, 1, 100, fp);
    fwrite(buffer, 1, 100, fp);
    fclose(fp);
    
    remove("temp.txt");
    rename("old.txt", "new.txt");
    
    FILE *tmp = tmpfile();
    fclose(tmp);
    
    putchar('\n');
    
    return 0;
}
