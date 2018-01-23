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
    call foo
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
foo:
    pushq %RBP
    movq %RSP, %RBP
    movq $10, %RAX
    leave
    ret
