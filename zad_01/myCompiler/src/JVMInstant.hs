module JVMInstant where

import AbsInstant
import Control.Monad.State
import Control.Monad.Writer
import Data.Map hiding (map)


type Variable = String
type Operation = String
type Local = Integer

data ProgramState = PS {
  locals :: Map Variable Local
}

type StateWriterMonad = StateT ProgramState (Writer [String])

initialState :: ProgramState
initialState = PS empty


addLocal :: String -> StateWriterMonad Local
addLocal variable = do
  PS lcls <- get
  if member variable lcls then do 
    return $ lcls ! variable else do
      let loc = toInteger $ (size lcls) + 1
      let newLcls = insert variable loc lcls 
      put (PS newLcls)
      return loc


getLocal :: String -> StateWriterMonad Local
getLocal variable = do
  PS lcls <- get
  return $ lcls ! variable


genProgram :: String -> Program -> StateWriterMonad ()
genProgram className (Prog stmts) = do
  localsCount <- countLocals stmts
  let stackHeight = stHgtStmts stmts
  genPrologue className localsCount stackHeight
  genStmts stmts
  genEpilogue


countLocals :: [Stmt] -> StateWriterMonad Integer
countLocals stmts = do
  mapM_ gatherStmt stmts
  PS locals <- get
  return (toInteger $ size locals)


gatherStmt :: Stmt -> StateWriterMonad ()
gatherStmt (SAss (Ident var) exp) = do
  addLocal var
  return ()
gatherStmt (SExp _) = return ()


stHgtStmts :: [Stmt] -> Integer
stHgtStmts stmts = maximum $ map stHgtStmt stmts


stHgtStmt :: Stmt -> Integer
stHgtStmt (SAss (Ident _) exp) = stHgtExp exp
stHgtStmt (SExp exp) = if st == 1 then 2 else st
  where st = stHgtExp exp


stHgtExp :: Exp -> Integer
stHgtExp (ExpAdd exp1 exp2) = stHgtBinExp exp1 exp2
stHgtExp (ExpSub exp1 exp2) = stHgtBinExp exp1 exp2
stHgtExp (ExpMul exp1 exp2) = stHgtBinExp exp1 exp2
stHgtExp (ExpDiv exp1 exp2) = stHgtBinExp exp1 exp2
stHgtExp (ExpLit _) = 1
stHgtExp (ExpVar _) = 1


stHgtBinExp :: Exp -> Exp -> Integer
stHgtBinExp exp1 exp2 = 
  if s1 == s2 then s1 + 1 else max s1 s2
    where
      s1 = stHgtExp exp1
      s2 = stHgtExp exp2


genPrologue :: String -> Integer -> Integer -> StateWriterMonad ()
genPrologue className localsCount stackHeight = do
  tell [ ".bytecode 52.0"
       , ".class " ++ className
       , ".super java/lang/Object"
       , ""
       , ".method <init>()V"
       , "  .limit stack 1"
       , "  .limit locals 1"
       , "  aload_0"
       , "  invokespecial java/lang/Object/<init>()V"
       , "  return"
       , ".end method"
       , ""
       , ".method public static main([Ljava/lang/String;)V"
       , "  .limit stack " ++ show stackHeight
       , "  .limit locals " ++ show (1 + localsCount)
       ]


genEpilogue :: StateWriterMonad ()
genEpilogue = do
  tell [ "  return"
       , ".end method" ]


genStmts :: [Stmt] -> StateWriterMonad ()
genStmts = mapM_ genStmt


genStmt :: Stmt -> StateWriterMonad ()
genStmt (SAss (Ident var) exp) = do
  loc <- getLocal var
  genExp exp
  emitAssignment loc
genStmt (SExp exp) =
  if stH == 1 
    then do
      emitGetStaticPrintStream
      genExp exp
      emitInvokePrintLn
    else do
      genExp exp
      emitPrintIntSwap
    where 
      stH = stHgtExp exp


genExp :: Exp -> StateWriterMonad ()
genExp (ExpAdd exp1 exp2) = genBinOperation "iadd" exp1 exp2
genExp (ExpSub exp1 exp2) = genBinOperationSwap "isub" exp1 exp2
genExp (ExpMul exp1 exp2) = genBinOperation "imul" exp1 exp2
genExp (ExpDiv exp1 exp2) = genBinOperationSwap "idiv" exp1 exp2
genExp (ExpLit int) = do 
  emitConst int
genExp (ExpVar (Ident var)) = do
  a <- getLocal var
  emitLoad a
    

genBinOperation :: Operation -> Exp -> Exp -> StateWriterMonad ()
genBinOperation op e1 e2 = do
  if s2 > s1 then do
    genExp e2
    genExp e1
    emitBinOperation op
  else do
    genExp e1
    genExp e2
    emitBinOperation op
  where 
    s1 = stHgtExp e1
    s2 = stHgtExp e2


genBinOperationSwap :: Operation -> Exp -> Exp -> StateWriterMonad ()
genBinOperationSwap op e1 e2 =
  if s2 > s1 then do
    genExp e2
    genExp e1
    emitSwap
    emitBinOperation op
  else do
    genExp e1
    genExp e2
    emitBinOperation op
  where 
    s1 = stHgtExp e1
    s2 = stHgtExp e2


emitPrintIntSwap :: StateWriterMonad ()
emitPrintIntSwap = do
  tell [ "  getstatic java/lang/System/out Ljava/io/PrintStream;"
       , "  swap"
       , "  invokevirtual java/io/PrintStream/println(I)V"]
       

emitGetStaticPrintStream :: StateWriterMonad ()
emitGetStaticPrintStream = do tell ["  getstatic java/lang/System/out Ljava/io/PrintStream;"]


emitInvokePrintLn :: StateWriterMonad ()
emitInvokePrintLn = do tell ["  invokevirtual java/io/PrintStream/println(I)V"]


emitConst :: Integer -> StateWriterMonad ()
emitConst int = do
  if int == -1
    then tell ["  iconst_m1"]
  else if int >=0 && int < 6
    then tell ["  iconst_" ++ show int]
  else do tell ["  ldc " ++ show int]


emitLoad :: Local -> StateWriterMonad ()
emitLoad loc = do
  if loc < 4 
    then tell ["  iload_" ++ show loc]
  else tell ["  iload " ++ show loc]


emitAssignment :: Local -> StateWriterMonad ()
emitAssignment loc = do
  if loc < 4 
    then tell ["  istore_" ++ show loc]
  else   tell ["  istore " ++ show loc]


emitBinOperation :: Operation -> StateWriterMonad ()
emitBinOperation op = do
  tell ["  " ++ op]


emitSwap :: StateWriterMonad ()
emitSwap = do
  tell ["  swap"]


compile :: String -> Program -> [String]
compile className prog = execWriter (evalStateT (genProgram className prog) initialState)
