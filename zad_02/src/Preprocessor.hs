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
    validateType vt t -- FIXME: no position info
validateStmt (Incr pos ident) = do
    VD t _ _ <- getVariable ident pos
    validateType (Bool Nothing) t -- FIXME: no position info
validateStmt (Decr pos ident) = do
    VD t _ _ <- getVariable ident pos
    validateType (Bool Nothing) t -- FIXME: no position info
validateStmt (Ret _ expr) = do
    et <- getReturnType
    t <- validateExpr expr
    validateType et t -- FIXME: no position info
validateStmt (VRet _) = do
    et <- getReturnType
    validateType et (Void Nothing) -- FIXME: no position info
validateStmt (Cond _ expr stmt) = do
    t <- validateExpr expr
    validateType (Bool Nothing) t -- FIXME: no position info
    validateStmt stmt
validateStmt (CondElse _ expr stmt1 stmt2) = do
    t <- validateExpr expr
    validateType (Bool Nothing) t -- FIXME: no position info
    validateStmt stmt1
    validateStmt stmt2
validateStmt (While _ expr stmt) = do
    t <- validateExpr expr
    validateType (Bool Nothing) t -- FIXME: no position info
    validateStmt stmt
validateStmt (SExp _ expr) = do 
    validateExpr expr
    return ()


validateItems :: Type Position -> [Item Position] -> PreprocessorMonad ()
validateItems vt = mapM_ (validateItem vt)

validateItem :: Type Position -> Item Position -> PreprocessorMonad ()
validateItem vt (NoInit _ ident) = addVariable vt ident
validateItem vt (Init _ ident expr) = do
    t <- validateExpr expr
    addVariable vt ident
    validateType vt t


validateExprs :: [Expr Position] -> PreprocessorMonad [Type Position]
validateExprs = mapM validateExpr

validateExpr :: Expr Position -> PreprocessorMonad (Type Position)
validateExpr (EVar pos ident) = do
    VD t _ _ <- getVariable ident pos
    return t
validateExpr (ELitInt _ _) = return $ Int Nothing
validateExpr (ELitTrue _) = return $ Bool Nothing
validateExpr (ELitFalse _) = return $ Bool Nothing
validateExpr (EApp pos ident exprs) = do
    FD rt eat _ <- getFunction ident pos
    at <- validateExprs exprs
    validateTypes eat at -- FIXME: no position info
    return rt
validateExpr (EString _ _) = return $ Str Nothing
validateExpr (Neg _ expr) = validateIntExpr expr
validateExpr (Not _ expr) = validateBoolExpr expr
validateExpr (EMul _ lexpr _ rexpr) = do
    validateIntExpr lexpr
    validateIntExpr rexpr
validateExpr (EAdd pos lexpr _ rexpr) = do
    lt <- validateIntOrStringExpr lexpr pos -- FIXME: `-` is not allowed for strings
    rt <- validateExpr rexpr
    validateType lt rt
    return lt
validateExpr (ERel pos lexpr _ rexpr) = do -- FIXME: EQU, NE can be applied to bools
    lt <- validateIntOrStringExpr lexpr pos
    rt <- validateExpr rexpr
    validateType lt rt
    return (Bool Nothing)
validateExpr (EAnd _ lexpr rexpr) = do
    validateBoolExpr lexpr
    validateBoolExpr rexpr
validateExpr (EOr _ lexpr rexpr) = do
    validateBoolExpr lexpr
    validateBoolExpr rexpr


validateIntOrStringExpr :: Expr Position -> Position -> PreprocessorMonad (Type Position)
validateIntOrStringExpr expr pos = do
    vt <- validateExpr expr
    let t = extractType vt
    let allowedTypes = [(Int Nothing), (Str Nothing)]
    unless (elem t allowedTypes) $
        throwError $ errorWithPosition pos ++ "type mismatch: "
        ++ "expected string or integer, found: " ++ showType t
    return t

validateIntExpr :: Expr Position -> PreprocessorMonad (Type Position)
validateIntExpr iexpr = do
    t <- validateExpr iexpr
    validateType (Int Nothing) t
    return (Int Nothing)

validateBoolExpr :: Expr Position -> PreprocessorMonad (Type Position)
validateBoolExpr bexpr = do
    t <- validateExpr bexpr
    validateType (Bool Nothing) t
    return (Bool Nothing)


validateTypes :: [Type Position] -> [Type Position] -> PreprocessorMonad ()
validateTypes = zipWithM_ validateType

validateType :: Type Position -> Type Position -> PreprocessorMonad ()
validateType et vt = do
    let (t, pos) = extractTypePosition vt
    unless (t == extractType et) $ 
        throwError $ errorWithPosition pos ++ " type mismatch, expected: "
            ++ showType et ++ ", found: " ++ showType t


collectFunctions :: [TopDef Position] -> PreprocessorMonad ()
collectFunctions = mapM_ collectFunction

collectFunction :: TopDef Position -> PreprocessorMonad ()
collectFunction = addFunction


analyzeProgram :: Program Position -> PreprocessorMonad (Program Position)
analyzeProgram tr@(Program _ topDefs) = do
    -- TODO: add predefined functions (could be in default state or here)
    collectFunctions topDefs
    validateFunctions topDefs
    return tr


preprocess :: Program Position -> AnalysisResult
preprocess p = evalState (runExceptT (analyzeProgram p)) emptyEnv
