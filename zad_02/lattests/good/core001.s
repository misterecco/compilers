main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq $10, %RDI
    call fac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call rfac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call mfac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $10, %RDI
    call ifac
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $1, %RDI
    movq $10, %R12
    jmp .lbl_1
.lbl_0:
    movq %RBX, %RDI
    imulq %R12, %RDI
    movq %R12, %RBX
    subq $1, %RBX
    movq %RBX, %R12
    jmp .lbl_1
.lbl_1:
    movq %RDI, %RBX
    cmpq $0, %R12
    jg .lbl_0
    jmp .lbl_2
.lbl_2:
    movq %RBX, %RDI
    call printInt
    movq str_0, %RDI
    movq $60, %RSI
    call repStr
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printString
    movq str_1, %RDI
    call printString
    movq str_2, %RDI
    call printString
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
fac:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %R12
    movq $1, %RDI
    jmp .lbl_4
.lbl_3:
    movq %RBX, %RDI
    imulq %R12, %RDI
    movq %R12, %RBX
    subq $1, %RBX
    movq %RBX, %R12
    jmp .lbl_4
.lbl_4:
    movq %RDI, %RBX
    cmpq $0, %R12
    jg .lbl_3
    jmp .lbl_5
.lbl_5:
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
rfac:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %RBX
    cmpq $0, %RDI
    je .lbl_6
    jmp .lbl_7
.lbl_6:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_7:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call rfac
    movq %RAX, %RDI
    movq %RBX, %R12
    imulq %RDI, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_8:
mfac:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %RBX
    cmpq $0, %RDI
    je .lbl_9
    jmp .lbl_10
.lbl_9:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_10:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call nfac
    movq %RAX, %RDI
    movq %RBX, %R12
    imulq %RDI, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_11:
nfac:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %RBX
    cmpq $0, %RDI
    jne .lbl_12
    jmp .lbl_13
.lbl_12:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call mfac
    movq %RAX, %RDI
    movq %RDI, %R12
    imulq %RBX, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_13:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_14:
ifac:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq $1, %RDI
    movq %RDI, %RSI
    call ifac2f
    movq %RAX, %R12
    movq %R12, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
ifac2f:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %R12
    movq %RSI, %RBX
    movq %RDI, %RBX
    cmpq %RSI, %RDI
    je .lbl_15
    jmp .lbl_16
.lbl_15:
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_16:
    cmpq %RBX, %R12
    jg .lbl_17
    jmp .lbl_18
.lbl_17:
    movq $1, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_18:
    movq %R12, %RDI
    addq %RBX, %RDI
    movq %RDI, %RAX
    cdqq
    idivq $2
    movq %RAX, %RSI
    pushq %RSI
    movq %R12, %RDI
    movq %RSI, %RSI
    call ifac2f
    popq %RSI
    movq %RAX, %RDI
    movq %RSI, %R12
    addq $1, %R12
    pushq %RDI
    movq %R12, %RDI
    movq %RBX, %RSI
    call ifac2f
    popq %RDI
    movq %RAX, %RSI
    movq %RDI, %RBX
    imulq %RSI, %RBX
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
repStr:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    pushq %R15
    subq $-8, %RSP
    movq %RDI, %R12
    movq str_3, %R13
    movq $0, %RDI
    jmp .lbl_20
.lbl_19:
    pushq %RDI
    pushq %RSI
    movq %RBX, %RDI
    movq %R12, %RSI
    call __concat__
    popq %RSI
    popq %RDI
    movq %RAX, %R13
    movq %RDI, %RBX
    addq $1, %RBX
    movq %RBX, %RDI
    jmp .lbl_20
.lbl_20:
    movq %R13, %RBX
    cmpq %RSI, %RDI
    jl .lbl_19
    jmp .lbl_21
.lbl_21:
    movq %RBX, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
