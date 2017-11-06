module InterpreterInstant where

import AbsInstant
import ErrM
import Control.Monad.State
import Data.Map


type Result = Err String

type Var = String

type ProgramState = State (Map Var Integer)


transProgram :: Program -> Result
transProgram (Prog stmts) = Ok $ evalState (foo stmts "") empty
  where
    foo :: [Stmt] -> String -> ProgramState String
    foo [] res = return res
    foo (s:ss) acc = do
      x <- transStmt s
      foo ss (acc ++ x)


transStmt :: Stmt -> ProgramState String
transStmt (SAss (Ident var) exp) = do
  x <- transExp exp
  env <- get
  put $ insert var x env
  return ""
transStmt (SExp exp) = do
  x <- transExp exp
  return $ (show x) ++ "\n"


transExp :: Exp -> ProgramState Integer
transExp (ExpAdd exp1 exp2) = binOp (+) exp1 exp2
transExp (ExpSub exp1 exp2) = binOp (-) exp1 exp2
transExp (ExpMul exp1 exp2) = binOp (*) exp1 exp2
transExp (ExpDiv exp1 exp2) = binOp div exp1 exp2
transExp (ExpLit integer) = return integer
transExp (ExpVar (Ident var)) = do
  env <- get
  return $ env ! var
    
binOp op e1 e2 = do
  x1 <- transExp e1
  x2 <- transExp e2
  return $ op x1 x2

