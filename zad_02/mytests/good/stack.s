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
    call f
    movq %RAX, %R13
    call f
    movq %RAX, %R14
    call f
    movq %RAX, %R15
    call f
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call f
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call f
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call f
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call f
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call f
    movq %RAX, %R11
    movq %RBX, %RDI
    addq %R12, %RDI
    movq %RDI, %RBX
    addq %R13, %RBX
    movq %RBX, %R12
    addq %R14, %R12
    movq %R12, %RBX
    addq %R15, %RBX
    movq %RBX, %R12
    addq 72(%RSP), %R12
    movq %R12, %RBX
    addq 64(%RSP), %RBX
    movq %RBX, %R12
    addq 56(%RSP), %R12
    movq %R12, %RBX
    addq 48(%RSP), %RBX
    movq %RBX, %R12
    addq 40(%RSP), %R12
    movq %R12, %RBX
    addq 32(%RSP), %RBX
    movq %RBX, %R12
    addq 24(%RSP), %R12
    movq %R12, %RBX
    addq 16(%RSP), %RBX
    movq %RBX, %R12
    addq 8(%RSP), %R12
    movq %R12, %RBX
    addq 0(%RSP), %RBX
    movq %RBX, %R12
    addq %R11, %R12
    movq %R12, %RDI
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
    movq $10, %RAX
    leave
    ret
