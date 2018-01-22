.section .data
.str_0:
    .string "Hello "
.str_1:
    .string "world!"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    leaq .str_0, %RDI
    leaq .str_1, %RSI
    call __concat__
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printString
    movq $0, %RAX
    popq %RBX
    leave
    ret
