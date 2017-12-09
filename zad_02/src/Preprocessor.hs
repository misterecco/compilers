module Preprocessor where

import AbsLatte
import PreState

import Control.Monad.Except
import Control.Monad.State


type AnalysisResult = Either String (Program Position)


validateFunctions :: [TopDef Position] -> PreprocessorMonad ()
validateFunctions = mapM_ validateFunction

validateFunction :: TopDef Position -> PreprocessorMonad ()
validateFunction (FnDef _ retType _ args block) =
    local enterBlock $ do
        setReturnType retType
        addArgs args
        validateBlock block


validateBlock :: Block Position -> PreprocessorMonad ()
validateBlock (Block _ stmts) = validateStmts stmts


validateStmts :: [Stmt Position] -> PreprocessorMonad ()
validateStmts = mapM_ validateStmt

validateStmt :: Stmt Position -> PreprocessorMonad ()
validateStmt (Empty _) = return ()
validateStmt (BStmt _ block) = local enterBlock $ validateBlock block
validateStmt (Decl _ vt items) = validateItems vt items
validateStmt (Ass pos ident expr) = do
    VD vt _ _ <- getVariable ident pos
    t <- validateExpr expr
    validateType pos vt t
validateStmt (Incr pos ident) = do
    VD t _ _ <- getVariable ident pos
    validateType pos NBool t
validateStmt (Decr pos ident) = do
    VD t _ _ <- getVariable ident pos
    validateType pos NBool t
validateStmt (Ret pos expr) = do
    et <- getReturnType
    t <- validateExpr expr
    validateType pos et t
validateStmt (VRet pos) = do
    et <- getReturnType
    validateType pos et NVoid
validateStmt (Cond pos expr stmt) = do
    t <- validateExpr expr
    validateType pos NBool t
    validateStmt stmt
validateStmt (CondElse pos expr stmt1 stmt2) = do
    t <- validateExpr expr
    validateType pos NBool t
    validateStmt stmt1
    validateStmt stmt2
validateStmt (While pos expr stmt) = do
    t <- validateExpr expr
    validateType pos NBool t
    validateStmt stmt
validateStmt (SExp _ expr) = do 
    validateExpr expr
    return ()


validateItems :: Type Position -> [Item Position] -> PreprocessorMonad ()
validateItems vt = mapM_ (validateItem vt)

validateItem :: Type Position -> Item Position -> PreprocessorMonad ()
validateItem vt (NoInit _ ident) = addVariable vt ident
validateItem vt (Init pos ident expr) = do
    t <- validateExpr expr
    addVariable vt ident
    let et = extractType vt
    validateType pos et t


validateExprs :: [Expr Position] -> PreprocessorMonad [NType]
validateExprs = mapM validateExpr

validateExpr :: Expr Position -> PreprocessorMonad NType
validateExpr (EVar pos ident) = do
    VD t _ _ <- getVariable ident pos
    return t
validateExpr (ELitInt _ _) = return NInt
validateExpr (ELitTrue _) = return NBool
validateExpr (ELitFalse _) = return NBool
validateExpr (EApp pos ident exprs) = do
    FD rt eat _ <- getFunction ident pos
    at <- validateExprs exprs
    validateTypes pos eat at
    return rt
validateExpr (EString _ _) = return NStr
validateExpr (Neg pos expr) = validateIntExpr pos expr
validateExpr (Not pos expr) = validateBoolExpr pos expr
validateExpr (EMul pos lexpr _ rexpr) = do
    validateIntExpr pos lexpr
    validateIntExpr pos rexpr
validateExpr (EAdd pos lexpr _ rexpr) = do
    lt <- validateIntOrStringExpr pos lexpr -- FIXME: `-` is not allowed for strings
    rt <- validateExpr rexpr
    validateType pos rt lt
    return lt
validateExpr (ERel pos lexpr _ rexpr) = do -- FIXME: EQU, NE can be applied to bools
    lt <- validateIntOrStringExpr pos lexpr
    rt <- validateExpr rexpr
    validateType pos rt lt
    return NBool
validateExpr (EAnd pos lexpr rexpr) = do
    validateBoolExpr pos lexpr
    validateBoolExpr pos rexpr
validateExpr (EOr pos lexpr rexpr) = do
    validateBoolExpr pos lexpr
    validateBoolExpr pos rexpr


validateIntOrStringExpr :: Position -> Expr Position -> PreprocessorMonad NType
validateIntOrStringExpr pos expr = do
    t <- validateExpr expr
    let allowedTypes = [NInt, NStr]
    unless (elem t allowedTypes) $
        throwError $ errorWithPosition pos ++ "type mismatch: "
        ++ "expected: string or integer, found: " ++ showNType t
    return t

validateIntExpr :: Position -> Expr Position -> PreprocessorMonad NType
validateIntExpr pos iexpr = do
    t <- validateExpr iexpr
    validateType pos NInt t
    return NInt

validateBoolExpr :: Position -> Expr Position -> PreprocessorMonad NType
validateBoolExpr pos bexpr = do
    t <- validateExpr bexpr
    validateType pos NBool t
    return NBool


validateTypes :: Position -> [NType] -> [NType] -> PreprocessorMonad ()
validateTypes pos et at = do
    unless (net == nat) $ throwError $ errorWithPosition pos ++ "type mismatch: "
        ++ "passing " ++ show nat ++ " argument(s), expected " ++ show net
        ++ " argument(s)" 
    zipWithM_ (validateType pos) et at
        where
            net = length et
            nat = length at

validateType :: Position -> NType -> NType -> PreprocessorMonad ()
validateType pos et t =
    unless (t == et) $ throwError $ errorWithPosition pos ++ "type mismatch: " 
        ++ "expected: " ++ showNType et ++ ", found: " ++ showNType t


collectFunctions :: [TopDef Position] -> PreprocessorMonad ()
collectFunctions = mapM_ collectFunction

collectFunction :: TopDef Position -> PreprocessorMonad ()
collectFunction = addFunction


verifyMain :: PreprocessorMonad ()
verifyMain = do
    m <- getFunction (Ident "main") Nothing
    unless (m == mainDef) $ throwError $ errorWithPosition (position m) 
        ++ "wrong type of `main`, should return int and take no arguments"


analyzeProgram :: Program Position -> PreprocessorMonad (Program Position)
analyzeProgram tr@(Program _ topDefs) = do
    collectFunctions topDefs
    verifyMain
    validateFunctions topDefs
    return tr


preprocess :: Program Position -> AnalysisResult
preprocess p = evalState (runExceptT (analyzeProgram p)) emptyEnv
