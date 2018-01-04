module CompState where

import Data.Map hiding (map, foldr)
import Control.Monad.State

type Label = String
type Var = String

data IRInstr 
    = IRAdd Var Var Var 
    | IRSUb Var Var Var 
    | IRMul Var Var Var
    | IRDiv Var Var Var
    | IRMod Var Var Var
    | IRAnd Var Var Var
    | IROr Var Var Var
    | IRLt Var Var Var
    | IRGt Var Var Var
    | IRLte Var Var Var
    | IRGte Var Var Var
    | IREq Var Var Var
    | IRNe Var Var Var
    | IRNeg Var Var
    | IRNot Var Var
    | IRInc Var Var
    | IRDec Var Var
    | IRCall Var Label [Var]
    | IRAndJmp Label Label Var Var
    | IROrJmp Label Label Var Var
    | IRRet Var
    deriving Show

data IRBlock = B {
    instructions :: [IRInstr],
    funArgs :: [Var],
    nextBlocks :: [Label],
    prevBlocks :: [Label]
}

data CompState = CS {
    blocks :: Map Label IRBlock,
    nextTemp :: Integer,
    nextLabel :: Integer
}


type CompMonad = State CompState

initialState :: CompState
initialState = CS empty 0 0


freshTemp :: CompMonad Var
freshTemp = do
    CS b nv nl <- get
    put $ CS b (nv+1) nl
    -- guaranteed not to clash with user input (vairable names)
    return $ "tmp-" ++ show nv

freshLabel :: CompMonad Label
freshLabel = do
    CS b nv nl <- get
    put $ CS b nv (nl+1)
    -- guaranteed not to clash with user input (function names)    
    return $ show nl

addBlock :: Label -> IRBlock -> CompMonad ()
addBlock label block = do
    CS bl nv nl <- get
    put $ CS (insert label block bl) nv nl

