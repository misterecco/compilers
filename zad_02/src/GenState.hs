module GenState where

import AbsLatte
import IRDef
import PreState ( Position )

import Control.Monad.State
import Control.Monad.Writer
import Data.Map

data GenState = CS {
    nextTemp :: Integer,
    nextLabel :: Integer,
    blockLevel :: Integer,
    variables :: Map Name IRVar,
    functions :: Map Label IRType
}

type IRGenMonad =  StateT GenState (Writer [IRInstr])

local :: (GenState -> GenState) -> IRGenMonad a -> IRGenMonad a
local modState comp = do
    initState@(CS _ _ bl vars funs) <- get
    let localState = modState initState
    put localState
    result <- comp
    CS nt nl _ _ _ <- get
    put $ CS nt nl bl vars funs
    return result

enterBlock :: GenState -> GenState
enterBlock (CS nv nl bl vars funs) =
    CS nv nl (bl+1) vars funs

getBlockLevel :: IRGenMonad Integer
getBlockLevel = do
    CS _ _ bl _ _ <- get
    return bl


builtInFunctions :: Map Label IRType
builtInFunctions = fromList 
    [ ("printInt", IRVoid)
    , ("printString", IRVoid) 
    , ("error", IRVoid)
    , ("readInt", IRInt)
    , ("readString", IRStr) ]

initialState :: GenState
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
