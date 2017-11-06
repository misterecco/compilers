module LLVMInstant where

import AbsInstant
import Control.Monad.State
import Control.Monad.Writer
import Data.Map


type Variable = String
type Operation = String

data Address = Immediate Integer | Indirect Variable

instance Show Address where
  show (Immediate x) = show x
  show (Indirect var) = "%" ++ var

data ProgramState = PS {
  nextTemp :: Integer,
  locals :: Map Variable Variable
}

type StateWriterMonad = StateT ProgramState (Writer [String])

initialState :: ProgramState
initialState = PS 0 empty


freshTemp :: StateWriterMonad String
freshTemp = do
  PS nt lcls <- get
  put (PS (nt+1) lcls)
  return $ "t_" ++ show nt


addLocal :: String -> StateWriterMonad Variable
addLocal variable = do
  PS nt lcls <- get
  if member variable lcls then do 
    return $ lcls ! variable else do
      let loc= "loc_" ++ variable
      emitLocal loc
      return loc
      let newLcls = insert variable loc lcls 
      put (PS nt newLcls)
      return loc


genLocal :: String -> StateWriterMonad Variable
genLocal variable = do
  PS _ lcls <- get
  return $ lcls ! variable


genProgram :: Program -> StateWriterMonad ()
genProgram (Prog stmts) = do
  genPrologue
  genStmts stmts
  genEpilogue


genPrologue :: StateWriterMonad ()
genPrologue = do
  tell [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
       , "declare i32 @printf(i8*, ...)"
       , ""
       , "define void @printInt(i32 %x) {"
       , "    %t = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
       , "    call i32 (i8*, ...) @printf(i8* %t, i32 %x)"
       , "    ret void"
       , "}"
       , ""
      --  , "declare void @printInt(i32)"
       , ""
       , "define i32 @main() {" ]


genEpilogue :: StateWriterMonad ()
genEpilogue = do
  tell [ ""
       , "    ret i32 0"
       , "}" ]


genStmts :: [Stmt] -> StateWriterMonad ()
genStmts = mapM_ genStmt


genStmt :: Stmt -> StateWriterMonad ()
genStmt (SAss (Ident var) exp) = do
  loc <- addLocal var
  addr <- genExp exp
  emitAssignment addr loc
genStmt (SExp exp) = do
  t <- genExp exp
  emitPrintInt t


genExp :: Exp -> StateWriterMonad Address
genExp (ExpAdd exp1 exp2) = genBinOperation "add" exp1 exp2
genExp (ExpSub exp1 exp2) = genBinOperation "sub" exp1 exp2
genExp (ExpMul exp1 exp2) = genBinOperation "mul" exp1 exp2
genExp (ExpDiv exp1 exp2) = genBinOperation "sdiv" exp1 exp2
genExp (ExpLit int) = do
  return $ Immediate int
genExp (ExpVar (Ident var)) = do
  a <- genLocal var
  t <- freshTemp
  emitLoad t a
  return $ Indirect t
    

genBinOperation :: Operation -> Exp -> Exp -> StateWriterMonad Address
genBinOperation op e1 e2 = do
  a1 <- genExp e1
  a2 <- genExp e2
  t <- freshTemp
  emitBinOperation t op a1 a2
  return $ Indirect t


emitPrintInt :: Address -> StateWriterMonad ()
emitPrintInt addr = do
  tell ["    call void @printInt(i32 " ++ show addr ++ ")"]

emitLoad :: Variable -> Variable -> StateWriterMonad ()
emitLoad var loc =
  tell ["    %" ++ var ++ " = load i32, i32* %" ++ loc]

emitAssignment :: Address -> Variable -> StateWriterMonad ()
emitAssignment addr loc = do
  tell ["    store i32 " ++ show addr ++ ", i32* %" ++ loc]

emitLocal :: Variable -> StateWriterMonad ()
emitLocal var = do
  tell ["    %" ++ var ++ " = alloca i32"]

emitBinOperation :: Variable -> Operation -> Address -> Address -> StateWriterMonad ()
emitBinOperation var op a1 a2 = do
  tell ["    %" ++ var ++ " = " ++ op ++ " i32 " ++ show a1 ++ ", " ++ show a2]


compile :: Program -> [String]
compile prog = execWriter (evalStateT (genProgram prog) initialState)
