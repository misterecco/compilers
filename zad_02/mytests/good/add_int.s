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
    pushq %R14
    pushq %R15
    call f
    movq %RAX, %RBX
    call f
    movq %RAX, %R12
    movq %RBX, %RDI
    call printInt
    movq %R12, %RDI
    call printInt
    movq %RBX, %RDI
    addq %R12, %RDI
    movq %RDI, %RBX
    addq %R12, %RBX
    pushq %RDI
    movq %RBX, %RDI
    call printInt
    popq %RDI
    movq %RDI, %RBX
    addq %R12, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
f:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq $10, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
