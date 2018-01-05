module CompState where

import AbsLatte
import PreState ( Position )

import Control.Monad.State
import Control.Monad.Writer
import Data.Map

type Label = String
type Name = String

data IRVar = IRVar Name Integer IRType
    deriving (Eq, Show)

data IRType = IRInt | IRStr | IRBool | IRVoid
    deriving (Eq, Show)

data IRAddr 
    = ImmInt Integer 
    | ImmString String 
    | ImmBool Bool 
    | NoRet
    | Indirect IRVar
    deriving (Eq, Show)

data IRInstr 
    = IRAss IROp IRAddr IRAddr IRAddr 
    | IRComp IRCmp IRAddr IRAddr IRAddr
    | IRSAss IRSOp IRAddr IRAddr
    | IRCall IRAddr Label [IRAddr]
    | IRIf IRCmp IRAddr IRAddr Label Label
    | IRGoto Label
    | IRCpy IRAddr IRAddr
    | IRLabel Label
    | IRRet IRAddr
    deriving Show

data IRCmp = IRGt | IRLt | IRGe | IRLe | IREq | IRNe
    deriving Show

data IROp = IRAdd | IRSub | IRMul | IRDiv | IRMod | IRAnd | IROr
    deriving Show

data IRSOp = IRNeg | IRNot | IRInc | IRDec
    deriving Show

data CompState = CS {
    nextTemp :: Integer,
    nextLabel :: Integer,
    blockLevel :: Integer,
    variables :: Map Name IRVar,
    functions :: Map Label IRType
}

type IRGenMonad =  StateT CompState (Writer [IRInstr])

builtInFunctions :: Map Label IRType
builtInFunctions = fromList 
    [ ("printInt", IRVoid)
    , ("printString", IRVoid) 
    , ("error", IRVoid)
    , ("readInt", IRInt)
    , ("readString", IRStr) ]

initialState :: CompState
initialState = CS 0 0 0 empty builtInFunctions


freshTemp :: IRType -> IRGenMonad IRAddr
freshTemp t =
    if t == IRVoid then 
        return NoRet else do
            CS nv nl bl vars funs <- get
            put $ CS (nv+1) nl bl vars funs
            -- guaranteed not to clash with user input (vairable names)
            let name = "tmp-" ++ show nv
            addVariable name t
            getVariable name


freshLabel :: IRGenMonad Label
freshLabel = do
    CS nv nl bl vars funs <- get
    put $ CS nv (nl+1) bl vars funs
    -- guaranteed not to clash with user input (function names)    
    return $ show nl


addVariable :: Name -> IRType -> IRGenMonad ()
addVariable n t = do
    CS nt nl bl vars funs <- get
    let nv = IRVar n bl t
    put $ CS nt nl bl (insert n nv vars) funs

getVariable :: Name -> IRGenMonad IRAddr
getVariable name = do
    CS _ _ _ vars _ <- get
    return $ Indirect (vars ! name)

addArgs :: [Arg Position] -> IRGenMonad ()
addArgs = mapM_ addArg

addArg :: Arg Position -> IRGenMonad ()
addArg (Arg _ vt (Ident ident)) = addVariable ident (extractType vt)

enterBlock :: CompState -> IRGenMonad CompState
enterBlock (CS nv nl bl vars funs) =
    return $ CS nv nl (bl+1) vars funs


addFunction :: TopDef Position -> IRGenMonad ()
addFunction (FnDef _ fnType (Ident ident) _ _) = do
    CS nt nl bs vars funs <- get
    let ft = extractType fnType
    put $ CS nt nl bs vars (insert ident ft funs)

getFunctionType :: Label -> IRGenMonad IRType
getFunctionType name = do
    CS _ _ _ _ funs <- get
    return $ funs ! name


extractType :: Type Position -> IRType
extractType (Int _) = IRInt
extractType (Str _) = IRStr
extractType (Bool _) = IRBool
extractType _ = IRVoid

addrType :: IRAddr -> IRType
addrType (ImmInt _) = IRInt
addrType (ImmString _) = IRStr
addrType (ImmBool _) = IRBool
addrType NoRet = IRVoid
addrType (Indirect (IRVar _ _ t)) = t


mulOpToIrOp :: MulOp Position -> IROp
mulOpToIrOp (Times _) = IRMul
mulOpToIrOp (Div _) = IRDiv
mulOpToIrOp (Mod _) = IRMod

addOpToIrOp :: AddOp Position -> IROp
addOpToIrOp (Plus _) = IRAdd
addOpToIrOp (Minus _) = IRSub

relOpToIrCmp :: RelOp Position -> IRCmp
relOpToIrCmp (LTH _) = IRLt
relOpToIrCmp (LE _) = IRLe
relOpToIrCmp (GTH _) = IRGt
relOpToIrCmp (GE _) = IRGe
relOpToIrCmp (EQU _) = IREq
relOpToIrCmp (NE _) = IRNe


-- data IRBlock = B {
--     instructions :: [IRInstr],
--     funArgs :: [Var],
--     nextBlocks :: [Label],
--     prevBlocks :: [Label]
-- }

-- addBlock :: Label -> IRBlock -> CompMonad ()
-- addBlock label block = do
--     CS bl nv nl <- get
--     put $ CS ((label, block) |< bl) nv nl
