llvm-as -o foo.bc foo.ll
llc -o foo.s foo.bc --x86-asm-syntax=intel
llvm-as -o runtime.bc runtime.ll
llc -o runtime.s runtime.bc --x86-asm-syntax=intel
gcc -o foo foo.s runtime.s -static

Llvm/runtime.ll

clang -O0 -o hello.ll -emit-llvm -S hello.c