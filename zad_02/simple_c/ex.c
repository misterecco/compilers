#include <stdio.h>

extern void printInt(int x);
extern void printString(char* str);
extern void error();
extern int readInt();
extern char* readString();

int main() {
    char* input = readString();

    printString("User input: ");
    printString(input);

    return 0;
}
