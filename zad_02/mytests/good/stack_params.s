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
    pushq $10
    pushq $9
    pushq $8
    pushq $7
    movq $1, %RDI
    movq $2, %RSI
    movq $3, %RDX
    movq $4, %RCX
    movq $5, %R8
    movq $6, %R9
    call f
    addq $32, %RSP
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    pushq $10
    pushq $9
    pushq $8
    pushq $7
    movq $1, %RDI
    movq $2, %RSI
    movq $3, %RDX
    movq $4, %RCX
    movq $5, %R8
    movq $6, %R9
    call h
    addq $32, %RSP
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $2, %RDI
    movq $4, %RSI
    movq $6, %RDX
    movq $8, %RCX
    movq $10, %R8
    movq $12, %R9
    call g
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
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
    movq %R12, %RBX
    addq 32(%RBP), %RBX
    movq %RBX, %R12
    addq 40(%RBP), %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
h:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq %R9, -8(%RBP)
    movq %RDI, %R11
    imulq %RSI, %R11
    movq %R11, %RBX
    imulq %RDX, %RBX
    movq %RBX, %R12
    imulq %RCX, %R12
    movq %R12, %RBX
    imulq %R8, %RBX
    movq %RBX, %R12
    imulq -8(%RBP), %R12
    movq %R12, %RBX
    imulq 16(%RBP), %RBX
    movq %RBX, %R12
    imulq 24(%RBP), %R12
    movq %R12, %RBX
    imulq 32(%RBP), %RBX
    movq %RBX, %R12
    imulq 40(%RBP), %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
g:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
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
    movq %R12, %RAX
    cqo
    movq $6, %R10
    idivq %R10
    movq %RAX, %RBX
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
