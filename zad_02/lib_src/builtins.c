#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(long long x) {
    printf("%lld\n", x);
}

void printString(const char* str) {
    printf("%s\n", str);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

char* readString() {
    char* buffer = NULL;
    size_t len = 0;

    size_t size = getline(&buffer, &len, stdin);
    if (size > 0) {
        buffer[size-1] = '\0';
    }

    return buffer;
}

long long readInt() {
    char* buffer = readString();
    long long x;

    sscanf(buffer, "%lld", &x);
    free(buffer);

    return x;
}

char* __concat__(const char* lhs, const char* rhs) {
    int len = strlen(lhs) + strlen(rhs) + 1;
    char* result = malloc(sizeof(char) * len);

    strcpy(result, lhs);
    strcat(result, rhs);

    return result;
}

int __strcmp__(const char* lhs, const char* rhs) {
    return strcmp(lhs, rhs);
}
