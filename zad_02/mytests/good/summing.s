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
    call readInt
    movq %RAX, %RBX
    call readInt
    movq %RAX, %R12
    call readInt
    movq %RAX, %R13
    call readInt
    movq %RAX, %R14
    call readInt
    movq %RAX, %R15
    call readInt
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call readInt
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call readInt
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    call readInt
    movq %RAX, %R11
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    movq %RAX, %R11
    pushq %R11
    pushq %R13
    pushq 40(%RSP)
    movq 56(%RSP), %RDI
    movq 72(%RSP), %RSI
    movq 56(%RSP), %RDX
    movq %R13, %RCX
    movq 32(%RSP), %R8
    movq 32(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 40(%RSP)
    pushq 16(%RSP)
    movq 64(%RSP), %RDI
    movq %R15, %RSI
    movq %R14, %RDX
    movq 16(%RSP), %RCX
    movq 16(%RSP), %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    pushq 24(%RSP)
    pushq %R12
    movq %R15, %RDI
    movq %R14, %RSI
    movq 48(%RSP), %RDX
    movq 72(%RSP), %RCX
    movq 48(%RSP), %R8
    movq 48(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq %R14
    pushq 8(%RSP)
    movq 16(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq 56(%RSP), %RDX
    movq %RBX, %RCX
    movq %RBX, %R8
    movq %R12, %R9
    call printSum
    addq $16, %RSP
    pushq 16(%RSP)
    pushq 48(%RSP)
    movq %R15, %RDI
    movq 24(%RSP), %RSI
    movq %R15, %RDX
    movq 56(%RSP), %RCX
    movq 72(%RSP), %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    pushq 48(%RSP)
    pushq 40(%RSP)
    movq %R13, %RDI
    movq %R13, %RSI
    movq 72(%RSP), %RDX
    movq %RBX, %RCX
    movq 64(%RSP), %R8
    movq 32(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq %R12
    pushq 24(%RSP)
    movq 24(%RSP), %RDI
    movq %R12, %RSI
    movq 56(%RSP), %RDX
    movq %R15, %RCX
    movq 56(%RSP), %R8
    movq %R12, %R9
    call printSum
    addq $16, %RSP
    pushq 32(%RSP)
    pushq %R14
    movq %R13, %RDI
    movq 32(%RSP), %RSI
    movq 16(%RSP), %RDX
    movq 72(%RSP), %RCX
    movq 72(%RSP), %R8
    movq %R14, %R9
    call printSum
    addq $16, %RSP
    pushq 48(%RSP)
    pushq 24(%RSP)
    movq %R13, %RDI
    movq %R12, %RSI
    movq 72(%RSP), %RDX
    movq %R13, %RCX
    movq 48(%RSP), %R8
    movq 32(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 56(%RSP)
    pushq %R12
    movq %R15, %RDI
    movq 72(%RSP), %RSI
    movq %RBX, %RDX
    movq 40(%RSP), %RCX
    movq %R15, %R8
    movq %R15, %R9
    call printSum
    addq $16, %RSP
    pushq 48(%RSP)
    pushq 16(%RSP)
    movq 56(%RSP), %RDI
    movq 72(%RSP), %RSI
    movq 24(%RSP), %RDX
    movq 48(%RSP), %RCX
    movq %R14, %R8
    movq 64(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 40(%RSP)
    pushq 48(%RSP)
    movq 56(%RSP), %RDI
    movq %RBX, %RSI
    movq 64(%RSP), %RDX
    movq 72(%RSP), %RCX
    movq 72(%RSP), %R8
    movq 72(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 48(%RSP)
    pushq %RBX
    movq 64(%RSP), %RDI
    movq %R13, %RSI
    movq 72(%RSP), %RDX
    movq 16(%RSP), %RCX
    movq 72(%RSP), %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    pushq 56(%RSP)
    pushq 64(%RSP)
    movq %RBX, %RDI
    movq %R12, %RSI
    movq %R12, %RDX
    movq 72(%RSP), %RCX
    movq 16(%RSP), %R8
    movq 64(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 8(%RSP)
    pushq 40(%RSP)
    movq 64(%RSP), %RDI
    movq 72(%RSP), %RSI
    movq 40(%RSP), %RDX
    movq 24(%RSP), %RCX
    movq 40(%RSP), %R8
    movq 72(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 48(%RSP)
    pushq 24(%RSP)
    movq %R12, %RDI
    movq %R14, %RSI
    movq 48(%RSP), %RDX
    movq 48(%RSP), %RCX
    movq 48(%RSP), %R8
    movq 56(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 8(%RSP)
    pushq %RBX
    movq 32(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq 72(%RSP), %RDX
    movq 24(%RSP), %RCX
    movq 40(%RSP), %R8
    movq 24(%RSP), %R9
    call printSum
    addq $16, %RSP
    pushq 8(%RSP)
    pushq 48(%RSP)
    movq 40(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq 16(%RSP), %RDX
    movq %R14, %RCX
    movq 48(%RSP), %R8
    movq %R12, %R9
    call printSum
    addq $16, %RSP
    pushq 8(%RSP)
    pushq 8(%RSP)
    movq %R13, %RDI
    movq %R15, %RSI
    movq %R12, %RDX
    movq %R13, %RCX
    movq %R13, %R8
    movq %R12, %R9
    call printSum
    addq $16, %RSP
    pushq %R14
    pushq 48(%RSP)
    movq 64(%RSP), %RDI
    movq 40(%RSP), %RSI
    movq 40(%RSP), %RDX
    movq 64(%RSP), %RCX
    movq 72(%RSP), %R8
    movq 48(%RSP), %R9
    call printSum
    addq $16, %RSP
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
printSum:
    pushq %RBP
    movq %RSP, %RBP
    addq $-24, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq %R9, -8(%RBP)
    movq %RDI, %R11
    addq %RSI, %R11
    movq %R11, %RBX
    addq %RDX, %RBX
    movq %RBX, %R12
    addq %RCX, %R12
    movq %R12, %RBX
    addq %R8, %RBX
    movq %RBX, %R12
    addq -8(%RBP), %R12
    movq %R12, %RBX
    addq 16(%RBP), %RBX
    movq %RBX, %R12
    addq 24(%RBP), %R12
    movq %R12, %RDI
    call printInt
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
