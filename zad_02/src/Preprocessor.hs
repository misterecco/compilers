module Preprocessor where

import AbsLatte
import PrintLatte

import Data.Map hiding (map)
import Data.List hiding (insert)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader


type Position = Maybe (Int, Int)

type AnalysisResult = Either String (Program Position)

data FunctionDef = FD {
    returnType :: Type Position,
    argumentTypes :: [Type Position],
    position :: Position
}
instance Eq FunctionDef where
    lhs == rhs = 
        returnType lhs == returnType rhs
        && argumentTypes rhs == argumentTypes lhs

data FuncEnv = FE {
    functions :: Map Ident FunctionDef
}

data Env = VE {
    variables :: Map Ident (Type Position),
    funRetType :: Type Position
}

type PreprocessorMonad = ExceptT String (ReaderT Env (State FuncEnv))


emptyEnv :: Env
emptyEnv = VE empty (Void Nothing)


emptyFuncEnv :: FuncEnv
emptyFuncEnv = FE empty

hasFunction :: Ident -> PreprocessorMonad Bool
hasFunction ident = do
    FE funcs <- get
    return $ member ident funcs

getFunction :: Ident -> Position -> PreprocessorMonad FunctionDef
getFunction ident pos = do
    FE funcs <- get
    verifyFunctionIsDefined ident pos
    return $ funcs ! ident
    
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

addFunction :: TopDef Position -> PreprocessorMonad ()
addFunction (FnDef pos fnType ident args _) = do
    FE funcs <- get
    verifyFunctionIsNotDefined ident pos
    verifyUniqueArguments args ident pos
    let stype = extractType fnType
    let sargs = extractArgumentTypes args
    put $ FE (insert ident (FD stype sargs pos) funcs)
    return ()

extractType :: Type Position -> Type Position
extractType (Int _) = Int Nothing
extractType (Str _) = Str Nothing
extractType (Bool _) = Bool Nothing
extractType (Void _) = Void Nothing
extractType (Fun _ rt ats) = Fun Nothing (extractType rt) (map extractType ats)

extractArgumentTypes :: [Arg Position] -> [Type Position]
extractArgumentTypes = map extractArgumentType

extractArgumentType :: Arg Position -> Type Position
extractArgumentType (Arg _ argType _) = extractType argType

extractArgumentIdent :: Arg Position -> Ident
extractArgumentIdent (Arg _ _ ident) = ident

extractArgumentIdents :: [Arg Position] -> [Ident]
extractArgumentIdents = map extractArgumentIdent


analyzeProgram :: Program Position -> PreprocessorMonad (Program Position)
analyzeProgram (Program pos topDefs) = do
    tr <- collectFunctions topDefs
    return $ Program pos tr

collectFunctions :: [TopDef Position] -> PreprocessorMonad [TopDef Position]
collectFunctions = mapM collectFunction

collectFunction :: TopDef Position -> PreprocessorMonad (TopDef Position)
collectFunction def = do
    addFunction def
    return def


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


preprocess :: Program Position -> AnalysisResult
preprocess p = evalState (runReaderT (runExceptT (analyzeProgram p)) emptyEnv) emptyFuncEnv
