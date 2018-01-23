.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-264, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    movq $0, %RBX
    movq $0, %R12
    movq $0, %R13
    movq $0, %R14
    movq $0, %R15
    movq $0, %R11
    movq $0, %RDI
    movq $0, %RSI
    movq $0, %RCX
    movq $0, %R8
    movq $0, %R9
    movq $0, -136(%RBP)
    movq $0, -128(%RBP)
    movq $0, -120(%RBP)
    movq $0, -112(%RBP)
    movq $0, -104(%RBP)
    movq $0, -96(%RBP)
    movq $0, -88(%RBP)
    movq $0, -80(%RBP)
    movq $0, -72(%RBP)
    movq $0, -64(%RBP)
    movq $0, -56(%RBP)
    movq $0, -48(%RBP)
    movq $0, -40(%RBP)
    movq $0, -32(%RBP)
    movq $0, -24(%RBP)
    movq $0, -16(%RBP)
    movq $0, -8(%RBP)
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R13
    addq $1, %R13
    movq %R13, %RBX
    addq %R13, %RBX
    movq %R13, %R14
    addq %RBX, %R14
    movq %R14, %R15
    imulq %R14, %R15
    movq %R13, %R11
    addq %R15, %R11
    movq %RBX, %RDI
    addq %R11, %RDI
    movq %R13, %RSI
    addq %R13, %RSI
    movq %R11, %RCX
    addq %R11, %RCX
    movq %RSI, %R8
    movq %R15, %R9
    movq %R12, -136(%RBP)
    movq %R12, -128(%RBP)
    movq $0, -120(%RBP)
    movq $0, -112(%RBP)
    movq $0, -104(%RBP)
    movq $0, -96(%RBP)
    movq $0, -88(%RBP)
    movq $0, -80(%RBP)
    movq $0, -72(%RBP)
    movq $0, -64(%RBP)
    movq $0, -56(%RBP)
    movq $0, -48(%RBP)
    movq $0, -40(%RBP)
    movq $0, -32(%RBP)
    movq $0, -24(%RBP)
    movq %R12, -16(%RBP)
    movq %R12, -8(%RBP)
    movq %RCX, %R12
    movq %R11, %RSI
    movq %R14, %R11
    movq %RDI, %RCX
    movq %R13, %R14
    movq %R15, %RDI
    movq %RBX, %R15
    movq %R13, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $10, %R12
    jl .t_lbl_0
    movq %R12, %RBX
    movq -128(%RBP), %R10
    movq %R10, -256(%RBP)
    movq -120(%RBP), %R10
    movq %R10, -248(%RBP)
    movq -112(%RBP), %R10
    movq %R10, -240(%RBP)
    movq -104(%RBP), %R10
    movq %R10, -232(%RBP)
    movq -96(%RBP), %R10
    movq %R10, -224(%RBP)
    movq -88(%RBP), %R10
    movq %R10, -216(%RBP)
    movq -80(%RBP), %R10
    movq %R10, -208(%RBP)
    movq -72(%RBP), %R10
    movq %R10, -200(%RBP)
    movq -64(%RBP), %R10
    movq %R10, -192(%RBP)
    movq -56(%RBP), %R10
    movq %R10, -184(%RBP)
    movq -48(%RBP), %R10
    movq %R10, -176(%RBP)
    movq -40(%RBP), %R10
    movq %R10, -168(%RBP)
    movq -32(%RBP), %R10
    movq %R10, -160(%RBP)
    movq -24(%RBP), %R10
    movq %R10, -152(%RBP)
    movq -16(%RBP), %R10
    movq %R10, -144(%RBP)
    movq %R13, %R12
    movq %R14, %R13
    movq %R15, %R14
    movq %R11, %R15
    movq %RDI, %R11
    movq %RSI, %RDI
    movq %RCX, %RSI
    movq %R8, %RCX
    movq %R9, %R8
    movq -8(%RBP), %R9
    jmp .lbl_2
.t_lbl_0:
    movq -136(%RBP), %R12
    jmp .lbl_0
.lbl_2:
    movq %R9, -264(%RBP)
    movq %R12, %R9
    addq %R13, %R9
    movq %R9, %R12
    addq %R14, %R12
    movq %R12, %R13
    addq %R15, %R13
    movq %R13, %R12
    addq %R11, %R12
    movq %R12, %R13
    addq %RDI, %R13
    movq %R13, %R12
    addq %RSI, %R12
    movq %R12, %R13
    addq %RCX, %R13
    movq %R13, %R12
    addq %RBX, %R12
    movq %R12, %RBX
    addq %R8, %RBX
    movq %RBX, %R12
    addq -264(%RBP), %R12
    movq %R12, %RBX
    addq -144(%RBP), %RBX
    movq %RBX, %R12
    addq -152(%RBP), %R12
    movq %R12, %RBX
    addq -160(%RBP), %RBX
    movq %RBX, %R12
    addq -168(%RBP), %R12
    movq %R12, %RBX
    addq -176(%RBP), %RBX
    movq %RBX, %R12
    addq -184(%RBP), %R12
    movq %R12, %RBX
    addq -192(%RBP), %RBX
    movq %RBX, %R12
    addq -200(%RBP), %R12
    movq %R12, %RBX
    addq -208(%RBP), %RBX
    movq %RBX, %R12
    addq -216(%RBP), %R12
    movq %R12, %RBX
    addq -224(%RBP), %RBX
    movq %RBX, %R12
    addq -232(%RBP), %R12
    movq %R12, %RBX
    addq -224(%RBP), %RBX
    movq %RBX, %R12
    addq -240(%RBP), %R12
    movq %R12, %RBX
    addq -248(%RBP), %RBX
    movq %RBX, %R12
    addq -256(%RBP), %R12
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
