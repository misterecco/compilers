.section .data
.str_0:
    .string "Hello world!"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    leaq .str_0, %RDI
    call printString
    movq $0, %RAX
    leave
    ret
