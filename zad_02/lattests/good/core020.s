.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    call p
    movq $1, %RDI
    call printInt
    movq $0, %RAX
    leave
    ret
p:
    pushq %RBP
    movq %RSP, %RBP
    leave
    ret
