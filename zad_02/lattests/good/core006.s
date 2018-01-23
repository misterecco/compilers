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
    movq $36, %RBX
    imulq $-1, %RBX
    movq $45, %RDI
    call printInt
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
