llvm-as -o foo.bc
llc -o foo.s foo.bc
llvm-as -o runtime.bc runtime.ll
llc -o runtime.s runtime.bs
gcc -o foo foo.s runtime.s 

/home/students/inf/PUBLIC/MRJP/Llvm/runtime.ll

clang -O0 -o hello.ll -emit-llvm -S hello.c