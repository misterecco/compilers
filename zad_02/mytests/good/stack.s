.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-40, %RSP
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
    movq %RAX, %RDI
    pushq %RDI
    call f
    popq %RDI
    movq %RAX, %RSI
    pushq %RDI
    pushq %RSI
    call f
    popq %RSI
    popq %RDI
    movq %RAX, %R13
    pushq %RDI
    pushq %RSI
    call f
    popq %RSI
    popq %RDI
    movq %RAX, %R14
    pushq %RDI
    pushq %RSI
    call f
    popq %RSI
    popq %RDI
    movq %RAX, %R15
    pushq %RDI
    pushq %RSI
    call f
    popq %RSI
    popq %RDI
    movq %RAX, %RCX
    pushq %RDI
    pushq %RSI
    pushq %RCX
    call f
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %R8
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    call f
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
    call f
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %RAX
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    call f
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %R10
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R10
    call f
    popq %R10
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, %R11
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R10
    pushq %R11
    call f
    popq %R11
    popq %R10
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -8(%RBP)
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R10
    pushq %R11
    call f
    popq %R11
    popq %R10
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -16(%RBP)
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R10
    pushq %R11
    call f
    popq %R11
    popq %R10
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -24(%RBP)
    movq %R11, -32(%RBP)
    movq %RBX, %R11
    addq %R12, %R11
    movq %R11, %RBX
    addq %RDI, %RBX
    movq %RBX, %R12
    addq %RSI, %R12
    movq %R12, %RBX
    addq %R13, %RBX
    movq %RBX, %R12
    addq %R14, %R12
    movq %R12, %RBX
    addq %R15, %RBX
    movq %RBX, %R12
    addq %RCX, %R12
    movq %R12, %RBX
    addq %R8, %RBX
    movq %RBX, %R12
    addq %R9, %R12
    movq %R12, %RBX
    addq %RAX, %RBX
    movq %RBX, %R12
    addq %R10, %R12
    movq %R12, %RBX
    addq -32(%RBP), %RBX
    movq %RBX, %R12
    addq -8(%RBP), %R12
    movq %R12, %RBX
    addq -16(%RBP), %RBX
    movq %RBX, %R12
    addq -24(%RBP), %R12
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
