module PreState where

import AbsLatte
import PrintLatte

import Data.Map hiding (map)
import Data.List hiding (insert)
import Control.Monad.Except
import Control.Monad.State


data NType = NInt | NStr | NBool | NVoid | NFun NType [NType]
    deriving (Eq)

type Position = Maybe (Int, Int)

data FunctionDef = FD {
    returnType :: NType,
    argumentTypes :: [NType],
    position :: Position
}
-- instance Eq FunctionDef where
--     lhs == rhs = 
--         returnType lhs == returnType rhs
--         && argumentTypes rhs == argumentTypes lhs

data VariableDef = VD {
    varType :: NType,
    varBlock :: Integer,
    varPos :: Position
}
-- instance Eq VariableDef where
--     lhs == rhs = varType lhs == varType lhs

data Env = E {
    functions :: Map Ident FunctionDef,
    variables :: Map Ident VariableDef,
    funRetType :: NType,
    blockLevel :: Integer
}

type PreprocessorMonad = ExceptT String (State Env)

emptyEnv :: Env
emptyEnv = E empty empty NVoid 0

local :: MonadState s m => (s -> m s) -> m a -> m a
local modState comp = do
    initialState <- get
    localState <- modState initialState
    put localState
    result <- comp
    put initialState
    return result


hasVariable :: Ident -> PreprocessorMonad Bool
hasVariable ident = do
    E _ vars _ _ <- get
    return $ member ident vars

getVariable :: Ident -> Position -> PreprocessorMonad VariableDef
getVariable ident pos = do
    E _ vars _ _ <- get
    verifyVariableIsDefined ident pos
    return $ vars ! ident

addVariable :: Type Position -> Ident -> PreprocessorMonad ()
addVariable vt ident = do
    (E funcs vars ret bl) <- get
    let (t, pos) = extractTypePosition vt
    verifyVariableIsNotDefined ident pos
    put $ E funcs (insert ident (VD t bl pos) vars) ret bl

verifyVariableIsDefined :: Ident -> Position -> PreprocessorMonad ()
verifyVariableIsDefined ident pos = do
    isDefined <- hasVariable ident
    unless isDefined $ 
        throwError $ errorWithPosition pos ++ 
        "variable " ++ showIdent ident ++ " not defined"

verifyVariableIsNotDefined :: Ident -> Position -> PreprocessorMonad ()
verifyVariableIsNotDefined ident pos = do
    isDefined <- hasVariable ident
    when isDefined $ do
        VD _ vbl prevPos <- getVariable ident pos
        bl <- getBlockLevel
        when (bl == vbl) $
            throwError $ errorWithPosition pos ++ "variable " ++ showIdent ident
                ++ " already defined at " ++ showPosition prevPos


addArgs :: [Arg Position] -> PreprocessorMonad ()
addArgs = mapM_ addArg

addArg :: Arg Position -> PreprocessorMonad ()
addArg (Arg _ vt ident) = addVariable vt ident


enterBlock :: Env -> PreprocessorMonad Env
enterBlock (E funcs vars retType bl) =
    return $ E funcs vars retType (bl + 1)

getBlockLevel :: PreprocessorMonad Integer
getBlockLevel = do
    E _ _ _ bl <- get
    return bl


setReturnType :: Type Position -> PreprocessorMonad ()
setReturnType retType = do
    let rt = extractType retType
    E funcs vars _ bl <- get
    put $ E funcs vars rt bl

getReturnType :: PreprocessorMonad NType
getReturnType = do
    E _ _ retType _ <- get
    return retType


hasFunction :: Ident -> PreprocessorMonad Bool
hasFunction ident = do
    E funcs _ _ _ <- get
    return $ member ident funcs

getFunction :: Ident -> Position -> PreprocessorMonad FunctionDef
getFunction ident pos = do
    E funcs _ _ _ <- get
    verifyFunctionIsDefined ident pos
    return $ funcs ! ident

addFunction :: TopDef Position -> PreprocessorMonad ()
addFunction (FnDef pos fnType ident args _) = do
    E funcs vars ret bl <- get
    verifyFunctionIsNotDefined ident pos
    verifyUniqueArguments args ident pos
    let stype = extractType fnType
    let sargs = extractArgumentTypes args
    put $ E (insert ident (FD stype sargs pos) funcs) vars ret bl
    
verifyFunctionIsDefined :: Ident -> Position -> PreprocessorMonad ()
verifyFunctionIsDefined ident pos = do
    isDefined <- hasFunction ident
    unless isDefined $ 
        throwError $ errorWithPosition pos ++ 
        "trying to call undefined function " ++ showIdent ident

verifyFunctionIsNotDefined :: Ident -> Position -> PreprocessorMonad ()
verifyFunctionIsNotDefined ident pos = do
    isDefined <- hasFunction ident
    when isDefined $ do
        FD _ _ prevPos <- getFunction ident pos
        throwError $ errorWithPosition pos ++ "function " ++ showIdent ident
                     ++ " already defined at " ++ showPosition prevPos

verifyUniqueArguments :: [Arg Position] -> Ident -> Position -> PreprocessorMonad ()
verifyUniqueArguments args funIdent pos = do
    let idents = extractArgumentIdents args
    let uniqueIdents = nub idents
    unless (length idents == length uniqueIdents) $ 
        throwError $ errorWithPosition pos ++ "duplicated argument in function "
        ++ showIdent funIdent


extractArgumentTypes :: [Arg Position] -> [NType]
extractArgumentTypes = map extractArgumentType

extractArgumentType :: Arg Position -> NType
extractArgumentType (Arg _ argType _) = extractType argType

extractArgumentIdents :: [Arg Position] -> [Ident]
extractArgumentIdents = map extractArgumentIdent

extractArgumentIdent :: Arg Position -> Ident
extractArgumentIdent (Arg _ _ ident) = ident

extractTypePosition :: Type Position -> (NType, Position)
extractTypePosition vt = (extractType vt, extractPosition vt)

extractType :: Type Position -> NType
extractType (Int _) = NInt
extractType (Str _) = NStr
extractType (Bool _) = NBool
extractType (Void _) = NVoid
extractType (Fun _ rt ats) = NFun (extractType rt) (map extractType ats)

extractPosition :: Type Position -> Position
extractPosition (Int pos) = pos
extractPosition (Str pos) = pos
extractPosition (Bool pos) = pos
extractPosition (Void pos) = pos
extractPosition (Fun pos _ _) = pos


errorWithPosition :: Position -> String
errorWithPosition Nothing = "Error: "
errorWithPosition loc@(Just _) = 
    "Error at " ++ showPosition loc ++ ": "

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = 
    "line " ++ show line ++ ", column " ++ show column

showIdent :: Ident -> String
showIdent ident = "`" ++ printTree ident ++ "`"

showType :: Type Position -> String
showType t = "`" ++ printTree t ++ "`"

showNType :: NType -> String
showNType NInt = "int"
showNType NStr = "string"
showNType NBool = "bool"
showNType NVoid = "void"
showNType (NFun rt args) = "function " ++ showNType rt ++ 
            " (" ++ unwords (intersperse ", " (map showNType args)) ++ ")"