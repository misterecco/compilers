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
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    leaq .str_0, %RDI
    leaq .str_1, %RSI
    call __concat__
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printString
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
