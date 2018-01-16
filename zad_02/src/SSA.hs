module SSA where

import IRDef
import CFG ( CFGBlock(..), CFGState(..) )

import Prelude hiding ( lookup )

import Control.Monad.State
import Data.List ( (\\), nub )
import Data.Maybe ( isNothing )
import Data.Map hiding ( (\\) )

data SSAState = SSAS {
    blockOrder :: [Label],
    blocks :: Map Label CFGBlock,
    varMapping :: Map (Label, IRAddr) IRAddr,
    nextVal :: Integer,
    unknowns :: Map Label (Map IRAddr IRAddr)
}

type SSAMonad = State SSAState

initialSSAState :: CFGState -> SSAState
initialSSAState (CFGS bo bl) = SSAS bo bl empty 0 empty

getBlockOrder :: SSAMonad [Label]
getBlockOrder = do
    SSAS bo _ _ _ _ <- get
    return bo

setBlock :: Label -> CFGBlock -> SSAMonad ()
setBlock lbl bl = do
    SSAS bo bs vm nv un <- get
    put $ SSAS bo (insert lbl bl bs) vm nv un

getBlock :: Label -> SSAMonad CFGBlock
getBlock lbl = do
    SSAS _ bls _ _ _ <- get
    return $ bls ! lbl

setMapping :: (Label, IRAddr) -> IRAddr -> SSAMonad ()
setMapping v val = do
    SSAS bo bl vm nv un <- get
    put $ SSAS bo bl (insert v val vm) nv un

getMapping :: (Label, IRAddr) -> SSAMonad (Maybe IRAddr)
getMapping v = do
    SSAS _ _ vm _ _ <- get
    return $ lookup v vm

transformMapping :: ((Label, IRAddr) -> IRAddr -> IRAddr) -> SSAMonad ()
transformMapping transFun = do
    SSAS bo bl vm nv un <- get
    put $ SSAS bo bl (mapWithKey transFun vm) nv un

freshVal :: IRAddr -> SSAMonad IRAddr
freshVal addr = let t = addrType addr in
    if t == IRVoid then 
        return NoRet 
    else do
        SSAS bo bl vm nv un <- get
        put $ SSAS bo bl vm (nv+1) un
        let name = "val-" ++ show nv
        return $ Indirect (IRVar name 0 t)

setUnknowns :: Label -> [(IRAddr, IRAddr)] -> SSAMonad ()
setUnknowns lbl vars = do
    SSAS bo bl um nv un <- get
    put $ SSAS bo bl um nv (insert lbl (fromList vars) un)

getUnknowns :: Label -> SSAMonad (Map IRAddr IRAddr)
getUnknowns lbl = do
    SSAS _ _ _ _ un <- get
    return $ un ! lbl

addToPhi :: Label -> IRAddr -> [(Label, IRAddr)] -> SSAMonad ()
addToPhi lbl var values = do
    B phi is nb pb <- getBlock lbl
    let newbl = B (insert var (nub values) phi) is nb pb
    setBlock lbl newbl


findMappings :: IRAddr -> [Label] -> [Label] -> Label -> SSAMonad [(Label, IRAddr)]
findMappings _ _ [] _ = return []
findMappings var vl (l:ls) lbl = do
    value <- getMapping (l, var)
    B _ _ _ pb <- getBlock l
    case value of
        Just val -> do
            nextMappings <- findMappings var evl ls lbl
            return $ (lbl, val):nextMappings
        Nothing -> findMappings var evl (ls ++ (pb \\ vl)) lbl
    where evl = l:vl

fm :: IRAddr -> [Label] -> [(Label, IRAddr)] -> SSAMonad [(Label, IRAddr)]
fm _ [] acc = return acc
fm var (l:ls) acc = do
    values <- findMappings var [] [l] l
    fm var ls (values ++ acc)

findValueRec :: IRAddr -> IRAddr -> Label -> Bool -> SSAMonad IRAddr
findValueRec var newvar lbl goUp = case var of
    Indirect _ -> do
        v <- getMapping (lbl, var)
        if goUp || isNothing v then do
                B _ _ _ pb <- getBlock lbl
                case pb of
                    [pr] -> findValueRec var newvar pr False
                    _ -> do
                        vals <- fm var pb []
                        case vals of
                            [(_, value)] -> return value
                            _ -> do
                                addToPhi lbl newvar vals
                                return newvar
        else do
            let Just val = v
            return val
    _ -> return var

findValue :: IRAddr -> Label -> Map IRAddr IRAddr -> SSAMonad IRAddr
findValue addr lbl unkn = case addr of
    Indirect _ -> 
        if addr `member` unkn then do
            let var = unkn ! addr
            val <- findValueRec var addr lbl True
            unless (val == addr) $ setMapping (lbl, addr) val
            transformMapping (\_ v -> if v == addr then val else v)
            setUnknowns lbl (toList $ delete addr unkn)
            return val
        else do
            val <- getMapping (lbl, addr)
            case val of
                Nothing -> return addr
                Just value -> return value
    _ -> return addr


getValue :: IRAddr -> Label -> [(IRAddr, IRAddr)] -> SSAMonad (IRAddr, [(IRAddr, IRAddr)])
getValue var lbl unkn = case var of
    Indirect _ -> do
        value <- getMapping (lbl, var)
        case value of
            Just val -> return (val, unkn)
            Nothing -> do
                newval <- freshVal var
                setMapping (lbl, var) newval
                return (newval, (newval, var):unkn)
    _ -> return (var, unkn)


firstPass :: Label -> [IRInstr] -> [IRInstr] -> [(IRAddr, IRAddr)] -> SSAMonad ()
firstPass lbl [] acc unkn = do
    let instrs = reverse acc
    B phi _ nb pb <- getBlock lbl
    setUnknowns lbl unkn
    setBlock lbl $ B phi instrs nb pb
firstPass lbl (i:is) acc un = case i of
    IRAss op dst larg rarg -> do
        (lval, un1) <- getValue larg lbl un
        (rval, un2) <- getValue rarg lbl un1
        newval <- freshVal dst
        setMapping (lbl, dst) newval
        let newinstr = IRAss op newval lval rval
        firstPass lbl is (newinstr:acc) un2
    IRSAss op dst arg -> do
        (argval, un1) <- getValue arg lbl un
        newval <- freshVal dst
        setMapping (lbl, dst) newval
        let newinstr = IRSAss op newval argval
        firstPass lbl is (newinstr:acc) un1
    IRCall dst fun args -> do
        (argvals, un2) <- foldM (\(a, p) ar -> do 
            (a1, un1) <- getValue ar lbl p
            return (a1:a, un1) ) ([], un) args
        newval <- freshVal dst
        setMapping (lbl, dst) newval
        let newinstr = IRCall newval fun (reverse argvals)
        unless (newval == NoRet) $ setMapping (lbl, dst) newval
        firstPass lbl is (newinstr:acc) un2
    IRIf cmp larg rarg lTrue lFalse -> do
        (lval, un1) <- getValue larg lbl un
        (rval, un2) <- getValue rarg lbl un1
        let newinstr = IRIf cmp lval rval lTrue lFalse
        firstPass lbl is (newinstr:acc) un2
    IRGoto l -> firstPass lbl is (IRGoto l:acc) un
    IRCpy dst arg -> do
        (argval, un1) <- getValue arg lbl un
        setMapping (lbl, dst) argval
        firstPass lbl is acc un1
    IRLabel l -> firstPass lbl is (IRLabel l:acc) un
    IRRet arg -> do
        (argval, un1) <- getValue arg lbl un
        let newinstr = IRRet argval
        firstPass lbl is (newinstr:acc) un1
    IRParam arg _ -> do
        setMapping (lbl, arg) arg
        firstPass lbl is (i:acc) un


secondPass :: Label -> [IRInstr] -> [IRInstr] -> SSAMonad ()
secondPass lbl [] acc = do
    let instrs = reverse acc
    B phi _ nb pb <- getBlock lbl
    setUnknowns lbl []
    setBlock lbl $ B phi instrs nb pb
secondPass lbl (i:is) acc = do
    unkn <- getUnknowns lbl
    case i of
        IRAss op dst larg rarg -> do
            lval <- findValue larg lbl unkn
            rval <- findValue rarg lbl unkn
            let newinstr = IRAss op dst lval rval
            secondPass lbl is (newinstr:acc)
        IRSAss op dst arg -> do
            argval <- findValue arg lbl unkn
            let newinstr = IRSAss op dst argval
            secondPass lbl is (newinstr:acc)
        IRCall dst fun args -> do
            argvals <- foldM (\a ar -> do 
                a1 <- findValue ar lbl unkn
                return (a1:a) ) [] args
            let newinstr = IRCall dst fun (reverse argvals)
            secondPass lbl is (newinstr:acc) 
        IRIf cmp larg rarg lTrue lFalse -> do
            lval <- findValue larg lbl unkn
            rval <- findValue rarg lbl unkn
            let newinstr = IRIf cmp lval rval lTrue lFalse
            secondPass lbl is (newinstr:acc)
        IRGoto l -> secondPass lbl is (IRGoto l:acc)
        IRCpy dst arg -> do
            argval <- findValue arg lbl unkn
            setMapping (lbl, dst) argval
            let newinstr = IRCpy dst argval
            secondPass lbl is (newinstr:acc)
        IRLabel l -> secondPass lbl is (IRLabel l:acc) 
        IRRet arg -> do
            argval <- findValue arg lbl unkn
            let newinstr = IRRet argval
            secondPass lbl is (newinstr:acc)       
        IRParam _ _ -> secondPass lbl is (i:acc)


addPhi :: Label -> SSAMonad ()
addPhi lbl = do
    B _ is _ _ <- getBlock lbl
    secondPass lbl is []

renumberVariables :: Label -> SSAMonad ()
renumberVariables lbl = do
    B _ is _ _ <- getBlock lbl
    firstPass lbl is [] []

transformBlocks :: [Label] -> SSAMonad ()
transformBlocks lbls = do
    mapM_ renumberVariables lbls
    mapM_ addPhi lbls


convertToSSA :: CFGState -> CFGState
convertToSSA st@(CFGS bo _) = CFGS nbo nbl where 
    (SSAS nbo nbl _ _ _) = execState (transformBlocks bo) (initialSSAState st)
