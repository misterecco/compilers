#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char* str) {
    printf("%s\n", str);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

int readInt() {
    int x;
    scanf("%d", &x);

    return x;
}

char* readString() {
    char* buffer = NULL;
    size_t len = 0;

    size_t size = getline(&buffer, &len, stdin);
    buffer[size-1] = '\0';

    return buffer;
}

char* __concat__(char* lhs, char* rhs) {
    int len = strlen(lhs) + strlen(rhs) + 1;
    char* result = malloc(sizeof(char) * len);

    strcpy(result, lhs);
    strcat(result, rhs);

    return result;
}
