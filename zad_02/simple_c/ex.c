#include <stdio.h>

extern void printInt(int x);
extern void printString(char* str);
extern void error();
extern int readInt();
extern char* readString();
extern char* __concat__(char*, char*);

int main() {
    printString("Please enter some text");
    char* input = readString();

    printString(__concat__("User input: ", input));

    return 0;
}
