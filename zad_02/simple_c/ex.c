#include <stdio.h>

extern void printInt(int x);
extern void printString(char* str);
extern void error();
extern int readInt();
extern char* readString();
extern char* __concat__(char*, char*);

int id(int x) { return x; }

int processSwaps(int Amount, int a, int b, int c, int d, int e) {
  int i = 0;
  while (i < Amount) {
    int t = a;
    a = e;
    e = d;
    d = c;
    c = b;
    b = t;
    i = i + 1;
    Amount = id(Amount);
  }

  return id(a + b * 2 + c * 3 + d * 4 + e * 5);
}


int main() {
    // int x = readInt();
    // printString("Please enter some text");
    // char* input = readString();
    // char* input2 = readString();

    // printInt(x);

    // printString(__concat__("User input: ", __concat__(input, input2)));

    printInt(processSwaps(3, 1, 2, 3, 4, 5));    

    return 0;
}