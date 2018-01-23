.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-56, %RSP
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
    popq %R11
    movq %RAX, %RDI
    pushq %RDI
    pushq %R11
    call f
    popq %R11
    popq %RDI
    movq %RAX, %RSI
    pushq %RDI
    pushq %RSI
    pushq %R11
    subq $8, %RSP
    call f
    addq $8, %RSP
    popq %R11
    popq %RSI
    popq %RDI
    movq %RAX, %RCX
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R11
    call f
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
    call f
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
    call f
    popq %R11
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
    pushq %R11
    call f
    popq %R11
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
    pushq %R11
    call f
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -24(%RBP)
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    call f
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -32(%RBP)
    pushq %RDI
    pushq %RSI
    pushq %RCX
    pushq %R8
    pushq %R9
    pushq %R11
    call f
    popq %R11
    popq %R9
    popq %R8
    popq %RCX
    popq %RSI
    popq %RDI
    movq %RAX, -40(%RBP)
    movq %R9, -48(%RBP)
    movq %RBX, %R9
    addq %R12, %R9
    movq %R9, %RBX
    addq %R13, %RBX
    movq %RBX, %R12
    addq %R14, %R12
    movq %R12, %RBX
    addq %R15, %RBX
    movq %RBX, %R12
    addq %R11, %R12
    movq %R12, %RBX
    addq %RDI, %RBX
    movq %RBX, %R12
    addq %RSI, %R12
    movq %R12, %RBX
    addq %RCX, %RBX
    movq %RBX, %R12
    addq %R8, %R12
    movq %R12, %RBX
    addq -48(%RBP), %RBX
    movq %RBX, %R12
    addq -8(%RBP), %R12
    movq %R12, %RBX
    addq -16(%RBP), %RBX
    movq %RBX, %R12
    addq -24(%RBP), %R12
    movq %R12, %RBX
    addq -32(%RBP), %RBX
    movq %RBX, %R12
    addq -40(%RBP), %R12
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
