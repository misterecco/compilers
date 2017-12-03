module Preprocessor where

import AbsLatte
import PrintLatte

import Data.Map hiding (map)
import Data.List hiding (insert)
import Control.Monad.Except
import Control.Monad.State


type Position = Maybe (Int, Int)

type AnalysisResult = Either String (Program Position)

data FunctionDef = FD {
    returnType :: Type Position,
    argumentTypes :: [Type Position],
    position :: Position
}
instance Eq FunctionDef where
    (FD lhrt lhargs _) == (FD rhrt rhargs _) = lhrt == rhrt && lhargs == rhargs

data ProgramState = PS {
    functions :: Map Ident FunctionDef,
    variables :: Map Ident (Type Position)
}

type ExceptStateMonad = ExceptT String (State ProgramState)


initialState :: ProgramState
initialState = PS empty empty

hasFunction :: Ident -> ExceptStateMonad Bool
hasFunction ident = do
    PS funcs _ <- get
    return $ member ident funcs

getFunction :: Ident -> Position -> ExceptStateMonad FunctionDef
getFunction ident pos = do
    PS funcs _ <- get
    verifyFunctionIsDefined ident pos
    return $ funcs ! ident
    
verifyFunctionIsDefined :: Ident -> Position -> ExceptStateMonad ()
verifyFunctionIsDefined ident pos = do
    isDefined <- hasFunction ident
    unless isDefined $ 
        throwError $ errorWithPosition pos ++ 
        "trying to call undefined function " ++ showIdent ident

verifyFunctionIsNotDefined :: Ident -> Position -> ExceptStateMonad ()
verifyFunctionIsNotDefined ident pos = do
    isDefined <- hasFunction ident
    when isDefined $ do
        FD _ _ prevPos <- getFunction ident pos
        throwError $ errorWithPosition pos ++ "function " ++ showIdent ident
                     ++ " already defined at " ++ showPosition prevPos

verifyUniqueArguments :: [Arg Position] -> Ident -> Position -> ExceptStateMonad ()
verifyUniqueArguments args funIdent pos = do
    let idents = extractArgumentIdents args
    let uniqueIdents = nub idents
    unless (length idents == length uniqueIdents) $ 
        throwError $ errorWithPosition pos ++ "duplicated argument in function "
        ++ showIdent funIdent

addFunction :: TopDef Position -> ExceptStateMonad ()
addFunction (FnDef pos fnType ident args _) = do
    PS funcs vars <- get
    verifyFunctionIsNotDefined ident pos
    verifyUniqueArguments args ident pos
    let stype = extractType fnType
    let sargs = extractArgumentTypes args
    put $ PS (insert ident (FD stype sargs pos) funcs) vars
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


analyzeProgram :: Program Position -> ExceptStateMonad (Program Position)
analyzeProgram (Program pos topDefs) = do
    tr <- collectFunctions topDefs
    return $ Program pos tr

collectFunctions :: [TopDef Position] -> ExceptStateMonad [TopDef Position]
collectFunctions = mapM collectFunction

collectFunction :: TopDef Position -> ExceptStateMonad (TopDef Position)
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
preprocess p = evalState (runExceptT (analyzeProgram p)) initialState