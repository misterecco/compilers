main:
  pushq %RBP
  movq %RSP, %RBP
  pushq %RBX
  pushq %R12
  pushq %R13
  pushq %R14
  pushq %R15
  subq $-8, %RSP
.lbl_0:
.lbl_1:
.lbl_2:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq $0, %RAX
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
.lbl_4:
.lbl_5:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
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
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq $1, %RAX
  leave
  ret
.lbl_7:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
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
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq $1, %RAX
  leave
  ret
.lbl_10:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
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
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
  leave
  ret
.lbl_13:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq $1, %RAX
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
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
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
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
  leave
  ret
.lbl_16:
.lbl_17:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq $1, %RAX
  leave
  ret
.lbl_18:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
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
.lbl_20:
.lbl_21:
  popq %R15
  popq %R14
  popq %R13
  popq %R12
  popq %RBX
  movq %RDI, %RAX
  leave
  ret
