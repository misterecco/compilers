main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
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
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    cmpq $1, %RDI
    je .t_lbl_0
    movq %RSI, %RBX
    movq %RDX, %R12
    jmp .lbl_1
.t_lbl_0:
    movq %RSI, %RBX
    movq %RDX, %R12
    jmp .lbl_0
.lbl_0:
    xchgq %RBX, %R12
    jmp .lbl_1
.lbl_1:
    movq %RBX, %RDI
    subq %R12, %RDI
    movq %RDI, %RDI
    call printInt
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
