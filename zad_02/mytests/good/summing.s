.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-24, %RSP
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
    popq %R11
    movq %RAX, %RDI
    pushq %RDI
    pushq %R11
    call readInt
    popq %R11
    popq %RDI
    movq %RAX, %RSI
    pushq %RDI
    pushq %RSI
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    popq %R11
    popq %RSI
    popq %RDI
    movq %RAX, %RCX
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R11
    call readInt
    popq %R11
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %R8
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R11
    subq $8, %RSP
    call readInt
    addq $8, %RSP
    popq %R11
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %R9
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    call readInt
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %R8, -8(%RBP)
    movq %RAX, %R8
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    call readInt
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %R12, -16(%RBP)
    movq %RAX, %R12
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq %R13
    pushq 32(%RSP)
    movq 48(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq 48(%RSP), %RDX
    movq %R13, %RCX
    movq 24(%RSP), %R8
    movq 24(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 32(%RSP)
    pushq 24(%RSP)
    movq 56(%RSP), %RDI
    movq %R15, %RSI
    movq %R14, %RDX
    movq %R12, %RCX
    movq %R12, %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq -8(%RBP)
    pushq -16(%RBP)
    movq %R15, %RDI
    movq %R14, %RSI
    movq 40(%RSP), %RDX
    movq 16(%RSP), %RCX
    movq 40(%RSP), %R8
    movq 40(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq %R14
    pushq %R12
    movq %R12, %RDI
    movq %R12, %RSI
    movq 48(%RSP), %RDX
    movq %RBX, %RCX
    movq %RBX, %R8
    movq -16(%RBP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 8(%RSP)
    pushq 40(%RSP)
    movq %R15, %RDI
    movq 32(%RSP), %RSI
    movq %R15, %RDX
    movq 48(%RSP), %RCX
    movq 16(%RSP), %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 40(%RSP)
    pushq 32(%RSP)
    movq %R13, %RDI
    movq %R13, %RSI
    movq 16(%RSP), %RDX
    movq %RBX, %RCX
    movq 56(%RSP), %R8
    movq 24(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq -16(%RBP)
    pushq 16(%RSP)
    movq 32(%RSP), %RDI
    movq -16(%RBP), %RSI
    movq 48(%RSP), %RDX
    movq %R15, %RCX
    movq 48(%RSP), %R8
    movq -16(%RBP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 24(%RSP)
    pushq %R14
    movq %R13, %RDI
    movq 24(%RSP), %RSI
    movq %R12, %RDX
    movq 16(%RSP), %RCX
    movq 16(%RSP), %R8
    movq %R14, %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 40(%RSP)
    pushq 16(%RSP)
    movq %R13, %RDI
    movq -16(%RBP), %RSI
    movq 16(%RSP), %RDX
    movq %R13, %RCX
    movq 40(%RSP), %R8
    movq 24(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 0(%RSP)
    pushq -16(%RBP)
    movq %R15, %RDI
    movq 16(%RSP), %RSI
    movq %RBX, %RDX
    movq -8(%RBP), %RCX
    movq %R15, %R8
    movq %R15, %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 40(%RSP)
    pushq 24(%RSP)
    movq 48(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq 32(%RSP), %RDX
    movq 40(%RSP), %RCX
    movq %R14, %R8
    movq 56(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 32(%RSP)
    pushq 40(%RSP)
    movq 48(%RSP), %RDI
    movq %RBX, %RSI
    movq 56(%RSP), %RDX
    movq 16(%RSP), %RCX
    movq 16(%RSP), %R8
    movq 16(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 40(%RSP)
    pushq %RBX
    movq 56(%RSP), %RDI
    movq %R13, %RSI
    movq 16(%RSP), %RDX
    movq %R12, %RCX
    movq 16(%RSP), %R8
    movq %RBX, %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 0(%RSP)
    pushq 8(%RSP)
    movq %RBX, %RDI
    movq -16(%RBP), %RSI
    movq -16(%RBP), %RDX
    movq 16(%RSP), %RCX
    movq %R12, %R8
    movq 56(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 16(%RSP)
    pushq 32(%RSP)
    movq 56(%RSP), %RDI
    movq 16(%RSP), %RSI
    movq -8(%RBP), %RDX
    movq 32(%RSP), %RCX
    movq -8(%RBP), %R8
    movq 16(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 40(%RSP)
    pushq 16(%RSP)
    movq -16(%RBP), %RDI
    movq %R14, %RSI
    movq 40(%RSP), %RDX
    movq 40(%RSP), %RCX
    movq 40(%RSP), %R8
    movq 48(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    pushq 16(%RSP)
    pushq %RBX
    movq 24(%RSP), %RDI
    movq %R12, %RSI
    movq 16(%RSP), %RDX
    movq 32(%RSP), %RCX
    movq -8(%RBP), %R8
    movq 32(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R11
    subq $8, %RSP
    pushq 16(%RSP)
    pushq 40(%RSP)
    movq -8(%RBP), %RDI
    movq %R12, %RSI
    movq %R12, %RDX
    movq %R14, %RCX
    movq 40(%RSP), %R8
    movq -16(%RBP), %R9
    call printSum
    addq $16, %RSP
    addq $8, %RSP
    popq %R11
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R11
    subq $8, %RSP
    pushq 16(%RSP)
    pushq %R12
    movq %R13, %RDI
    movq %R15, %RSI
    movq -16(%RBP), %RDX
    movq %R13, %RCX
    movq %R13, %R8
    movq -16(%RBP), %R9
    call printSum
    addq $16, %RSP
    addq $8, %RSP
    popq %R11
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R11
    pushq %R14
    pushq 24(%RSP)
    movq 40(%RSP), %RDI
    movq -8(%RBP), %RSI
    movq -8(%RBP), %RDX
    movq 40(%RSP), %RCX
    movq 16(%RSP), %R8
    movq 24(%RSP), %R9
    call printSum
    addq $16, %RSP
    popq %R11
    popq %RCX
    popq %RSI
    popq %RDI
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
