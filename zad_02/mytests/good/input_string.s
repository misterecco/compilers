.section .data
.str_0:
    .string "Please enter a string"
.str_1:
    .string "Your input is"
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
    call printString
    call readString
    movq %RAX, %RBX
    leaq .str_1, %RDI
    call printString
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
