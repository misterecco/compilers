module Generator where

import AbsLatte
import CompState
import PreState ( Position, local )

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


toIrItems :: Type Position -> [Item Position] -> IRGenMonad ()
toIrItems vt = mapM_ (toIrItem (extractType vt))

toIrItem :: IRType -> Item Position -> IRGenMonad ()
toIrItem vt (NoInit _ (Ident ident)) = addVariable ident vt
toIrItem vt (Init _ (Ident ident) expr) = do
    addr <- toIrExpr expr
    addVariable ident vt
    var <- getVariable ident
    emitCpy var addr

toIrExprs :: [Expr Position] -> IRGenMonad [IRAddr]
toIrExprs = mapM toIrExpr

toIrExpr :: Expr Position -> IRGenMonad IRAddr
toIrExpr (EVar _ (Ident ident)) = getVariable ident
toIrExpr (ELitInt _ value) = return $ ImmInt value
toIrExpr (ELitTrue _) = return $ ImmBool True
toIrExpr (ELitFalse _) = return $ ImmBool False
toIrExpr (EApp _ (Ident fun) exprs) = do
    paramAddrs <- toIrExprs exprs
    funType <- getFunctionType fun
    addr <- freshTemp funType
    emitCall addr fun paramAddrs
    return addr
toIrExpr (EString _ value) = return $ ImmString value
toIrExpr (Neg _ expr) = toSAssExpr IRNeg expr
toIrExpr (Not _ expr) = toSAssExpr IRNot expr
toIrExpr (EMul _ lexpr op rexpr) = toAssExpr (mulOpToIrOp op) lexpr rexpr
toIrExpr (EAdd _ lexpr op rexpr) = toAssExpr (addOpToIrOp op) lexpr rexpr


toAssExpr :: IROp -> Expr Position -> Expr Position -> IRGenMonad IRAddr
toAssExpr op lexpr rexpr = do
    lexpAddr <- toIrExpr lexpr
    rexpAddr <- toIrExpr rexpr
    let t = addrType lexpAddr
    addr <- freshTemp t
    emitAss op addr lexpAddr rexpAddr
    return addr

toSAssExpr :: IRSOp -> Expr Position -> IRGenMonad IRAddr
toSAssExpr op expr = do
    exprAddr <- toIrExpr expr
    let t = addrType exprAddr
    addr <- freshTemp t
    emitSAss op addr exprAddr
    return addr

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
