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
    pushq %R12
    pushq %R13
    call f
    movq %RAX, %RBX
    call f
    movq %RAX, %R12
    movq %RBX, %RDI
    call printInt
    movq %R12, %RDI
    call printInt
    movq %RBX, %R13
    addq %R12, %R13
    movq %R13, %RBX
    addq %R12, %RBX
    movq %RBX, %RDI
    call printInt
    movq %R13, %RBX
    addq %R12, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
f:
    pushq %RBP
    movq %RSP, %RBP
    addq $0, %RSP
    movq $10, %RAX
    leave
    ret
