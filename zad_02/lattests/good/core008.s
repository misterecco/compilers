.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    movq $1234234, %RBX
    imulq $-1, %RBX
    movq %RBX, %RDI
    call printInt
    movq $7, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
