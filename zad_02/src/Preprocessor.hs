module Preprocessor where

import AbsLatte
import PreState

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe


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
    validateType pos NInt t
validateStmt (Decr pos ident) = do
    VD t _ _ <- getVariable ident pos
    validateType pos NInt t
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
    local enterBlock $ validateStmt stmt
validateStmt (CondElse pos expr stmt1 stmt2) = do
    t <- validateExpr expr
    validateType pos NBool t
    local enterBlock $ validateStmt stmt1
    local enterBlock $ validateStmt stmt2
validateStmt (While pos expr stmt) = do
    t <- validateExpr expr
    validateType pos NBool t
    local enterBlock $ validateStmt stmt
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
validateExpr (EAdd pos lexpr op rexpr) = case op of
    (Plus _) -> validatePairOneOf pos lexpr rexpr [NInt, NStr]
    (Minus _) -> do
        validateIntExpr pos lexpr
        validateIntExpr pos rexpr
validateExpr (ERel pos lexpr op rexpr) = case op of
    (EQU _) -> do
        validatePairOneOf pos lexpr rexpr[NInt, NBool, NStr]
        return NBool
    (NE _) -> do
        validatePairOneOf pos lexpr rexpr[NInt, NBool, NStr]
        return NBool
    _ -> do
        validatePairOneOf pos lexpr rexpr[NInt, NStr]
        return NBool
validateExpr (EAnd pos lexpr rexpr) = do
    validateBoolExpr pos lexpr
    validateBoolExpr pos rexpr
validateExpr (EOr pos lexpr rexpr) = do
    validateBoolExpr pos lexpr
    validateBoolExpr pos rexpr

validatePairOneOf :: Position -> Expr Position -> Expr Position -> [NType] -> PreprocessorMonad NType
validatePairOneOf pos lexpr rexpr at = do
    lt <- validateOneOf pos lexpr at
    rt <- validateExpr rexpr
    validateType pos rt lt
    return lt

validateOneOf :: Position -> Expr Position -> [NType] -> PreprocessorMonad NType
validateOneOf pos expr allowedTypes = do
    t <- validateExpr expr
    unless (elem t allowedTypes) $
        throwError $ errorWithPosition pos ++ "type mismatch: expected one of: " 
        ++ showNTypes allowedTypes ++ ", found: " ++ showNType t
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


optimizeFunctions :: [TopDef Position] -> [TopDef Position]
optimizeFunctions = map optimizeFunction

optimizeFunction :: TopDef Position -> TopDef Position
optimizeFunction (FnDef pos retType ident args block) =
    FnDef pos retType ident args (optimizeBlock block)


optimizeBlock :: Block Position -> Block Position
optimizeBlock (Block pos stmts) = Block pos (optimizeStmts stmts)


optimizeStmts :: [Stmt Position] -> [Stmt Position]
optimizeStmts = mapMaybe optimizeStmt

optimizeStmt :: Stmt Position -> Maybe (Stmt Position)
optimizeStmt (Empty _) = Nothing
optimizeStmt (BStmt pos block) = Just $ BStmt pos (optimizeBlock block)
optimizeStmt s@(Cond _ expr stmt) = case expr of
    ELitTrue _ -> Just stmt
    ELitFalse _ -> Nothing
    _ -> Just s
optimizeStmt s@(CondElse _ expr stmt1 stmt2) = case expr of
    ELitTrue _ -> Just stmt1
    ELitFalse _ -> Just stmt2
    _ -> Just s
optimizeStmt s@(While _ expr _) = case expr of
    ELitFalse _ -> Nothing
    _ -> Just s
optimizeStmt s = Just s


checkReturnFunctions :: [TopDef Position] -> PreprocessorMonad ()
checkReturnFunctions = mapM_ checkReturnFunction

checkReturnFunction :: TopDef Position -> PreprocessorMonad ()
checkReturnFunction (FnDef pos retType ident args block) =
    case retType of
        Void _ -> return ()
        _ -> unless (checkReturnBlock block) $ throwError $ errorWithPosition pos
                ++ "missing return in function " ++ showIdent ident 
                ++ ", function should return " ++ showType retType


checkReturnBlock :: Block Position -> Bool
checkReturnBlock (Block _ stmts) = checkReturnStmts stmts


checkReturnStmts :: [Stmt Position] -> Bool
checkReturnStmts [] = False
checkReturnStmts (st:sts) = checkReturnStmt st || checkReturnStmts sts

checkReturnStmt :: Stmt Position -> Bool
checkReturnStmt (BStmt _ block) = checkReturnBlock block
checkReturnStmt s@(CondElse _ expr stmt1 stmt2) = 
    checkReturnStmt stmt1 && checkReturnStmt stmt2
checkReturnStmt (Ret _ _) = True
checkReturnStmt _ = False


analyzeProgram :: Program Position -> PreprocessorMonad (Program Position)
analyzeProgram (Program pos topDefs) = do
    collectFunctions topDefs
    verifyMain
    validateFunctions topDefs
    let optTr = optimizeFunctions topDefs
    checkReturnFunctions optTr
    return $ Program pos optTr


preprocess :: Program Position -> AnalysisResult
preprocess p = evalState (runExceptT (analyzeProgram p)) emptyEnv
