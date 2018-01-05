module GenState where

import AbsLatte
import PreState ( Position )

import Control.Monad.State
import Control.Monad.Writer
import Data.Map hiding (map)
import Data.List ( intercalate )

type Label = String
type Name = String

data IRVar = IRVar Name Integer IRType
    deriving Eq

instance Show IRVar where
    show (IRVar name int irtype) = show irtype ++ " " ++ name ++ " " ++ show int

data IRType = IRInt | IRStr | IRBool | IRVoid
    deriving Eq

instance Show IRType where
    show IRInt = "int"
    show IRStr = "string"
    show IRBool = "bool"
    show IRVoid = "void"

data IRAddr 
    = ImmInt Integer 
    | ImmString String 
    | ImmBool Bool 
    | NoRet
    | Indirect IRVar
    deriving Eq

instance Show IRAddr where
    show (ImmInt val) = "Literal " ++ show val
    show (ImmString val) = "Literal " ++ val
    show (ImmBool val) = "Literal " ++ show val
    show NoRet = "NoRet"
    show (Indirect val) = "Indirect " ++ show val

data IRInstr 
    = IRAss IROp IRAddr IRAddr IRAddr 
    | IRSAss IRSOp IRAddr IRAddr
    | IRCall IRAddr Label [IRAddr]
    | IRIf IRCmp IRAddr IRAddr Label Label
    | IRGoto Label
    | IRCpy IRAddr IRAddr
    | IRLabel Label
    | IRRet IRAddr
    | IRParam IRVar

instance Show IRInstr where
    show (IRAss op dst l r) = 
        show dst ++ " := " ++ show l ++ " " ++ show op ++ " " ++ show r
    show (IRSAss op dst r) =
        show dst ++ " := " ++ show op ++ " " ++ show r    
    show (IRCall dst fun args) =
        show dst ++ " := " ++ fun ++ "(" ++ showArgs args ++ ")"
    show (IRIf op l r lTrue lFalse) =
        "if " ++ show l ++ " " ++ show op ++ " " ++ show r
        ++ " then " ++ lTrue ++ " else " ++ lFalse
    show (IRGoto lbl) = "goto " ++ lbl
    show (IRCpy dst src) = show dst ++ " := " ++ show src
    show (IRLabel lbl) = lbl ++ ":"
    show (IRRet addr) = "return " ++ show addr
    show (IRParam addr) = "parameter " ++ show addr

data IRCmp = IRGt | IRLt | IRGe | IRLe | IREq | IRNe

instance Show IRCmp where
    show IRGt = ">"
    show IRLt = "<"
    show IRGe = ">="
    show IRLe = "<="
    show IREq = "=="
    show IRNe = "!="

data IROp = IRAdd | IRSub | IRMul | IRDiv | IRMod

instance Show IROp where
    show IRAdd = "+"
    show IRSub = "-"
    show IRMul = "*"
    show IRDiv = "/"
    show IRMod = "%"

data IRSOp = IRNeg | IRNot

instance Show IRSOp where
    show IRNeg = "-"
    show IRNot = "!"

data CompState = CS {
    nextTemp :: Integer,
    nextLabel :: Integer,
    blockLevel :: Integer,
    variables :: Map Name IRVar,
    functions :: Map Label IRType
}

type IRGenMonad =  StateT CompState (Writer [IRInstr])

local :: (CompState -> CompState) -> IRGenMonad a -> IRGenMonad a
local modState comp = do
    initState@(CS _ _ bl vars funs) <- get
    let localState = modState initState
    put localState
    result <- comp
    CS nt nl _ _ _ <- get
    put $ CS nt nl bl vars funs
    return result

enterBlock :: CompState -> CompState
enterBlock (CS nv nl bl vars funs) =
    CS nv nl (bl+1) vars funs

getBlockLevel :: IRGenMonad Integer
getBlockLevel = do
    CS _ _ bl _ _ <- get
    return bl


showArgs :: Show a => [a] -> String
showArgs args = intercalate ", " (map show args)

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
