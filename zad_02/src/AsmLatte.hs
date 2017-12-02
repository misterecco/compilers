module SkelLatte where

import AbsLatte
import ErrM
type Result = Err String
    
failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
    Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
    Program _ topdefs -> failure x
transTopDef :: Show a => TopDef a -> Result
transTopDef x = case x of
    FnDef _ type_ ident args block -> failure x
transArg :: Show a => Arg a -> Result
transArg x = case x of
    Arg _ type_ ident -> failure x
transBlock :: Show a => Block a -> Result
transBlock x = case x of
    Block _ stmts -> failure x
transStmt :: Show a => Stmt a -> Result
transStmt x = case x of
    Empty _ -> failure x
    BStmt _ block -> failure x
    Decl _ type_ items -> failure x
    Ass _ ident expr -> failure x
    Incr _ ident -> failure x
    Decr _ ident -> failure x
    Ret _ expr -> failure x
    VRet _ -> failure x
    Cond _ expr stmt -> failure x
    CondElse _ expr stmt1 stmt2 -> failure x
    While _ expr stmt -> failure x
    SExp _ expr -> failure x
transItem :: Show a => Item a -> Result
transItem x = case x of
    NoInit _ ident -> failure x
    Init _ ident expr -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
    Int _ -> failure x
    Str _ -> failure x
    Bool _ -> failure x
    Void _ -> failure x
    Fun _ type_ types -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
    EVar _ ident -> failure x
    ELitInt _ integer -> failure x
    ELitTrue _ -> failure x
    ELitFalse _ -> failure x
    EApp _ ident exprs -> failure x
    EString _ string -> failure x
    Neg _ expr -> failure x
    Not _ expr -> failure x
    EMul _ expr1 mulop expr2 -> failure x
    EAdd _ expr1 addop expr2 -> failure x
    ERel _ expr1 relop expr2 -> failure x
    EAnd _ expr1 expr2 -> failure x
    EOr _ expr1 expr2 -> failure x
transAddOp :: Show a => AddOp a -> Result
transAddOp x = case x of
    Plus _ -> failure x
    Minus _ -> failure x
transMulOp :: Show a => MulOp a -> Result
transMulOp x = case x of
    Times _ -> failure x
    Div _ -> failure x
    Mod _ -> failure x
transRelOp :: Show a => RelOp a -> Result
transRelOp x = case x of
    LTH _ -> failure x
    LE _ -> failure x
    GTH _ -> failure x
    GE _ -> failure x
    EQU _ -> failure x
    NE _ -> failure x

    