#include <stdio.h>

extern void printInt(int x);
extern void printString(char* str);
extern void error();
extern int readInt();
extern char* readString();
extern char* __concat__(char*, char*);

int main() {
    int x = readInt();
    printString("Please enter some text");
    char* input = readString();
    char* input2 = readString();

    printInt(x);

    printString(__concat__("User input: ", __concat__(input, input2)));

    return 0;
}
