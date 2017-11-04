.bytecode 52.0
.class Fib
.super java/lang/Object

.method <init>()V
  .limit stack 1
  .limit locals 1
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 2
  .limit locals 1
  getstatic java/lang/System/out Ljava/io/PrintStream;
  bipush 10
  invokestatic Fib/fibR(I)I
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method

.method static fibR(I)I
  .limit stack 3
  .limit locals 1
  iload_0
  iconst_2
  if_icmpgt ELSE
  iconst_1
  ireturn
ELSE:
  iload_0
  iconst_1
  isub
  invokestatic Fib/fibR(I)I
  iload_0
  iconst_2
  isub
  invokestatic Fib/fibR(I)I
  iadd
  ireturn
  ;.stack
  ;  offset 7
  ;  .end stack
.end method
