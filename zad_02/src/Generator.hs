module Generator where

import AbsLatte
import CompState
import PreState ( Position )

import Control.Monad.State
import Control.Monad.Writer


toIrProgram :: Program Position -> IRGenMonad ()
toIrProgram (Program _ topDefs) = do
    collectFunctions topDefs
    toIrFunctions topDefs

collectFunctions :: [TopDef Position] -> IRGenMonad ()
collectFunctions = mapM_ addFunction

toIrFunctions :: [TopDef Position] -> IRGenMonad ()
toIrFunctions = mapM_ toIrFunction

toIrFunction :: TopDef Position -> IRGenMonad ()
toIrFunction (FnDef _ _ (Ident label) args block) = do
    emitLabel label
    local enterBlock $ do
        addArgs args
        toIrBlock block

toIrBlock :: Block Position -> IRGenMonad ()
toIrBlock (Block _ stmts) = toIrStmts stmts

toIrStmts :: [Stmt Position] -> IRGenMonad ()
toIrStmts = mapM_ toIrStmt

toIrStmt :: Stmt Position -> IRGenMonad ()
toIrStmt (Empty _) = return ()
toIrStmt (BStmt _ block) = local enterBlock $ toIrBlock block
toIrStmt (Decl _ vt items) = toIrItems vt items
toIrStmt (Ass _ (Ident ident) expr) = do
    addr <- toIrExpr Nothing expr
    varAddr <- getVariable ident
    emitCpy varAddr addr
toIrStmt (Incr _ (Ident ident)) = do
    varAddr <- getVariable ident
    emitAss IRAdd varAddr varAddr (ImmInt 1)
toIrStmt (Decr _ (Ident ident)) = do
    varAddr <- getVariable ident
    emitAss IRSub varAddr varAddr (ImmInt 1)
toIrStmt (Ret _ expr) = do
    addr <- toIrExpr Nothing expr
    emitRet addr
toIrStmt (VRet _) = emitRet NoRet
toIrStmt (Cond _ expr stmt) = do
    lTrue <- freshLabel
    lFalse <- freshLabel
    _ <- toIrExpr (Just (lTrue, lFalse)) expr
    emitLabel lTrue
    local enterBlock $ toIrStmt stmt
    emitLabel lFalse
toIrStmt (CondElse _ expr sTrue sFalse) = do
    lTrue <- freshLabel
    lFalse <- freshLabel
    lEnd <- freshLabel
    _ <- toIrExpr (Just (lTrue, lFalse)) expr
    emitLabel lTrue
    local enterBlock $ toIrStmt sTrue
    emitGoto lEnd
    emitLabel lFalse
    local enterBlock $ toIrStmt sFalse
    emitGoto lEnd    
    emitLabel lEnd
toIrStmt (While _ expr stmt) = do
    lLoop <- freshLabel
    lCond <- freshLabel
    lEnd <- freshLabel
    emitGoto lCond
    emitLabel lLoop
    local enterBlock $ toIrStmt stmt
    emitGoto lCond    
    emitLabel lCond
    _ <- toIrExpr (Just (lLoop, lEnd)) expr
    emitLabel lEnd
toIrStmt (SExp _ expr) = do
    _ <- toIrExpr Nothing expr
    return ()


toIrItems :: Type Position -> [Item Position] -> IRGenMonad ()
toIrItems vt = mapM_ (toIrItem (extractType vt))

toIrItem :: IRType -> Item Position -> IRGenMonad ()
toIrItem vt (NoInit _ (Ident ident)) = addVariable ident vt
toIrItem vt (Init _ (Ident ident) expr) = do
    addr <- toIrExpr Nothing expr
    addVariable ident vt
    var <- getVariable ident
    emitCpy var addr

toIrExprs :: [Expr Position] -> IRGenMonad [IRAddr]
toIrExprs = mapM (toIrExpr Nothing)

toIrExpr :: Maybe (Label, Label) -> Expr Position -> IRGenMonad IRAddr
toIrExpr _ (EVar _ (Ident ident)) = getVariable ident
toIrExpr _ (ELitInt _ value) = return $ ImmInt value
toIrExpr lbls (ELitTrue _) = case lbls of
    Nothing -> return $ ImmBool True
    Just (lTrue, _) -> do
        emitGoto lTrue
        return NoRet
toIrExpr lbls (ELitFalse _) = case lbls of
    Nothing -> return $ ImmBool False
    Just (_, lFalse) -> do
        emitGoto lFalse
        return NoRet
toIrExpr lbls (EApp _ (Ident fun) exprs) = do
    paramAddrs <- toIrExprs exprs
    funType <- getFunctionType fun
    addr <- freshTemp funType
    emitCall addr fun paramAddrs
    case lbls of
        Nothing -> return addr
        Just (lTrue, lFalse) -> do
            emitCmpJmp IREq addr (ImmBool True) lTrue lFalse
            return NoRet
toIrExpr _ (EString _ value) = return $ ImmString value
toIrExpr _ (Neg _ expr) = toSAssExpr IRNeg expr
toIrExpr lbls (Not _ expr) = case lbls of
    Nothing -> toSAssExpr IRNot expr
    Just (lTrue, lFalse) -> toIrExpr (Just (lFalse, lTrue)) expr
toIrExpr _ (EMul _ lexpr op rexpr) = toAssExpr (mulOpToIrOp op) lexpr rexpr
toIrExpr _ (EAdd _ lexpr op rexpr) = toAssExpr (addOpToIrOp op) lexpr rexpr
toIrExpr lbls e@(ERel _ lexpr op rexpr) = case lbls of
    Nothing -> toIrExprAddAssgmnt lbls e
    Just (lTrue, lFalse) -> do
        laddr <- toIrExpr Nothing lexpr
        raddr <- toIrExpr Nothing rexpr
        emitCmpJmp (relOpToIrCmp op) laddr raddr lTrue lFalse
        return NoRet
toIrExpr lbls e@(EAnd _ lexpr rexpr) = case lbls of
    Nothing -> toIrExprAddAssgmnt lbls e
    Just (lTrue, lFalse) -> do
        lMid <- freshLabel
        toIrExpr (Just (lMid, lFalse)) lexpr
        emitLabel lMid
        toIrExpr (Just (lTrue, lFalse)) rexpr
        return NoRet
toIrExpr lbls e@(EOr _ lexpr rexpr) = case lbls of
    Nothing -> toIrExprAddAssgmnt lbls e
    Just (lTrue, lFalse) -> do
        lMid <- freshLabel
        toIrExpr (Just (lTrue, lMid)) lexpr
        emitLabel lMid
        toIrExpr (Just (lTrue, lFalse)) rexpr
        return NoRet

toIrExprAddAssgmnt :: Maybe (Label, Label) -> Expr Position -> IRGenMonad IRAddr
toIrExprAddAssgmnt _ e = do
    lTrue <- freshLabel
    lFalse <- freshLabel
    toIrExpr (Just (lTrue, lFalse)) e
    addr <- freshTemp IRBool
    emitBoolAss addr lTrue lFalse
    return addr

toAssExpr :: IROp -> Expr Position -> Expr Position -> IRGenMonad IRAddr
toAssExpr op lexpr rexpr = do
    lexpAddr <- toIrExpr Nothing lexpr
    rexpAddr <- toIrExpr Nothing rexpr
    let t = addrType lexpAddr
    addr <- freshTemp t
    emitAss op addr lexpAddr rexpAddr
    return addr

toSAssExpr :: IRSOp -> Expr Position -> IRGenMonad IRAddr
toSAssExpr op expr = do
    exprAddr <- toIrExpr Nothing expr
    let t = addrType exprAddr
    addr <- freshTemp t
    emitSAss op addr exprAddr
    return addr


emitRet :: IRAddr -> IRGenMonad ()
emitRet addr = tell [IRRet addr]

emitGoto :: Label -> IRGenMonad ()
emitGoto label = tell [IRGoto label]

emitCmpJmp :: IRCmp -> IRAddr -> IRAddr -> Label -> Label -> IRGenMonad ()
emitCmpJmp cmp laddr raddr lTrue lFalse =
    tell [IRIf cmp laddr raddr lTrue lFalse]

emitBoolAss :: IRAddr -> Label -> Label -> IRGenMonad ()
emitBoolAss addr lTrue lFalse = do
    lEnd <- freshLabel
    tell 
        [ IRLabel lTrue
        , IRCpy addr (ImmBool True)
        , IRGoto lEnd
        , IRLabel lFalse
        , IRCpy addr (ImmBool False)
        , IRGoto lEnd
        , IRLabel lEnd ]

emitAss :: IROp -> IRAddr -> IRAddr -> IRAddr -> IRGenMonad ()
emitAss op dst laddr raddr = tell [IRAss op dst laddr raddr]

emitSAss :: IRSOp -> IRAddr -> IRAddr -> IRGenMonad ()
emitSAss op dst src = tell [IRSAss op dst src]

emitCall :: IRAddr -> Label -> [IRAddr] -> IRGenMonad ()
emitCall dst name params = tell [IRCall dst name params]

emitCpy :: IRAddr -> IRAddr -> IRGenMonad ()
emitCpy dst src = tell [IRCpy dst src]

emitLabel :: Label -> IRGenMonad ()
emitLabel label = tell [IRLabel label]


generateIR :: Program Position -> [IRInstr]
generateIR p = execWriter (execStateT (toIrProgram p) initialState)
