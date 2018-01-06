module CFG where

import IRDef

import Control.Monad.State
import Data.List ( intercalate )
import Data.Map hiding ( map )

type Phi = Map IRAddr [(Label, IRAddr)]

data CFGBlock = B {
    blockPhi :: Phi,
    instructions :: [IRInstr],
    nextBlocks :: [Label],
    prevBlocks :: [Label]
}

instance Show CFGBlock where
    show (B phi instrs nb pb) = unlines 
        [ "Before: " ++ intercalate ", " pb
        , "After: " ++ intercalate ", " nb
        , "Phi: " ++ show (toList phi)
        , "Instructions: "
        , unlines (map show instrs) ]

data CFGState = CFGS {
    blockOrder :: [Label],
    blocks :: Map Label CFGBlock
}

type CFGMonad = State CFGState

initialCFGState :: CFGState
initialCFGState = CFGS [] empty

addNewBlock :: Label -> [IRInstr] -> CFGMonad ()
addNewBlock label instrs = do
    CFGS bo bs <- get
    put $ CFGS bo (insert label (B empty instrs [] []) bs)

setBlock :: Label -> CFGBlock -> CFGMonad ()
setBlock lbl bl = do
    CFGS bo bs <- get
    put $ CFGS bo (insert lbl bl bs)

getBlock :: Label -> CFGMonad CFGBlock
getBlock lbl = do
    CFGS _ bls <- get
    return $ bls ! lbl

setBlockOrder :: [Label] -> CFGMonad ()
setBlockOrder bo = do
    CFGS _ bs <- get
    put $ CFGS bo bs

getBlockOrder :: CFGMonad [Label]
getBlockOrder = do
    CFGS bo _ <- get
    return bo

addEdge :: Label -> Label -> CFGMonad ()
addEdge src dst = do
    B sp si snb spb <- getBlock src
    B dp di dnb dpb <- getBlock dst
    setBlock src (B sp si (dst:snb) spb)
    setBlock dst (B dp di dnb (src:dpb))


cbHelper :: [IRInstr] -> [IRInstr] -> [Label] -> CFGMonad ()
cbHelper [] [] bo = do
    let ordering = reverse bo
    setBlockOrder ordering
cbHelper [] cb bo = do
    let instrs = reverse cb
    let (IRLabel lbl) = head instrs
    addNewBlock lbl instrs
    cbHelper [] [] bo
cbHelper (i:is) cb bo = case i of
    IRLabel l -> do
        when (length cb > 0) $ do
            let instrs = reverse cb
            let (IRLabel lbl) = head instrs
            addNewBlock lbl instrs
        cbHelper is [i] (l:bo)
    _ -> cbHelper is (i:cb) bo


findEdges :: Label -> CFGMonad ()
findEdges lbl = do
    B _ instrs _ _ <- getBlock lbl
    let i = last instrs
    case i of
        IRGoto dst -> addEdge lbl dst
        IRIf _ _ _ dst1 dst2 -> do
            addEdge lbl dst1
            addEdge lbl dst2
        _ -> return ()


createBlocks :: [IRInstr] -> CFGMonad ()
createBlocks instrs = do
    cbHelper instrs [] []
    bo <- getBlockOrder
    mapM_ findEdges bo


generateCFG :: [IRInstr] -> CFGState
generateCFG instrs = execState (createBlocks instrs) initialCFGState
