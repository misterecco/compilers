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
    movq %RAX, %RDI
    movq %RDI, %RDI
    call printInt
    movq $10, %RDI
    call rfac
    movq %RAX, %RDI
    movq %RDI, %RDI
    call printInt
    movq $10, %RDI
    call mfac
    movq %RAX, %RDI
    movq %RDI, %RDI
    call printInt
    movq $10, %RDI
    call ifac
    movq %RAX, %RDI
    movq %RDI, %RDI
    call printInt
.lbl_0:
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RCX, %RSI
    subq $1, %RSI
.lbl_1:
.lbl_2:
    movq %RDI, %RDI
    call printInt
    movq str_0, %RDI
    movq $60, %RSI
    call repStr
    movq %RAX, %RDI
    movq %RDI, %RDI
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
.lbl_3:
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RCX, %RSI
    subq $1, %RSI
.lbl_4:
.lbl_5:
    movq %RDI, %RAX
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
    movq %RSI, %RDI
    subq $1, %RDI
    pushq %RSI
    movq %RDI, %RDI
    call rfac
    popq %RSI
    movq %RAX, %RCX
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RDI, %RAX
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
    movq %RSI, %RDI
    subq $1, %RDI
    pushq %RSI
    movq %RDI, %RDI
    call nfac
    popq %RSI
    movq %RAX, %RCX
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RDI, %RAX
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
.lbl_12:
    movq %RSI, %RDI
    subq $1, %RDI
    pushq %RSI
    movq %RDI, %RDI
    call mfac
    popq %RSI
    movq %RAX, %RCX
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RDI, %RAX
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
    movq %RAX, %RSI
    movq %RDI, %RAX
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
.lbl_15:
    movq %RDI, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
.lbl_16:
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
    movq %RSI, %RDI
    addq %RCX, %RDI
    movq %R8, %RAX
    cdqq
    idivq $2
    movq %RAX, %RDI
    pushq %RDI
    pushq %RCX
    movq %RSI, %RDI
    movq %RDI, %RSI
    call ifac2f
    popq %RCX
    popq %RDI
    movq %RAX, %R8
    movq %RSI, %RDI
    addq $1, %RDI
    pushq %R8
    movq %RDI, %RDI
    movq %RSI, %RSI
    call ifac2f
    popq %R8
    movq %RAX, %RCX
    movq %RSI, %RDI
    imulq %RCX, %RDI
    movq %RDI, %RAX
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
.lbl_19:
    movq %RDI, %RDI
    movq %RSI, %RSI
    call __concat__
    popq %RSI
    movq %RAX, %RCX
    movq %R8, %RDI
    addq $1, %RDI
.lbl_20:
.lbl_21:
    movq %RDI, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
