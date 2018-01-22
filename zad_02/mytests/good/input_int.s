.section .data
.str_0:
    .string "Please enter a number"
.str_1:
    .string "Your number is"
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
    call printString
    call readInt
    movq %RAX, %RBX
    leaq .str_1, %RDI
    call printString
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
