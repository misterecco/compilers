.section .data
.str_0:
    .string "foo"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    call foo
    movq $0, %RAX
    leave
    ret
foo:
    pushq %RBP
    movq %RSP, %RBP
    leaq .str_0, %RDI
    call printString
    leave
    ret
