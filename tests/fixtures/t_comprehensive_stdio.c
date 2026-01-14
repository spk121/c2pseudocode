#include <stdio.h>

int main(void) {
    FILE *fp;
    int count = 0;
    char buffer[256];
    
    // Open a file for writing
    fp = fopen("output.txt", "w");
    if (fp == NULL) {
        printf("Error: Could not open file\n");
        return 1;
    }
    
    // Write some data using fprintf
    fprintf(fp, "Hello, World!\n");
    fprintf(fp, "Counter value: %d\n", count);
    fprintf(fp, "Multiple values: %d, %s, %f\n", 42, "test", 3.14);
    
    // Close the file
    fclose(fp);
    
    // Open file for reading
    fp = fopen("output.txt", "r");
    
    // Read data using various functions
    fgets(buffer, sizeof(buffer), fp);
    printf("Read line: %s", buffer);
    
    // Seek to beginning
    fseek(fp, 0, SEEK_SET);
    
    // Get position
    long pos = ftell(fp);
    printf("Current position: %ld\n", pos);
    
    // Read binary data
    fread(buffer, 1, 10, fp);
    
    // Close file
    fclose(fp);
    
    // More printf examples
    printf("Simple message\n");
    printf("Number: %d\n", count);
    printf("String: %s, Number: %d, Float: %f\n", "hello", 123, 45.67);
    
    return 0;
}
