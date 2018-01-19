module CGDef where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveBlock() )


data CGReg = RBX | R12 | R13 | R14 | R15 | RAX | R10 | R11 |
             RDI | RSI | RDX | RCX | R8 | R9 | RSP | RBP
    deriving (Show, Eq, Ord)

data CGMem = Reg CGReg | Mem CGReg Integer | Lit CGImm | Obj Label
    deriving (Eq, Ord)

instance Show CGMem where
    show m = case m of
        Reg reg -> "%" ++ show reg
        Mem reg int -> show int ++ "(%" ++ show reg ++ ")"
        Lit lit -> show lit
        Obj lbl -> lbl

data CGImm = CGBool Bool | CGInt Integer
    deriving (Eq, Ord)

instance Show CGImm where
    show lit = case lit of
        CGBool True -> "$1"
        CGBool False -> "$0"
        CGInt int -> "$" ++ show int

data AsmInstr 
    = Mov CGMem CGMem
    | Lea CGMem CGMem
    | Add CGMem CGMem
    | Sub CGMem CGMem
    | Imul CGMem CGMem
    | Idiv CGMem
    | Cqo
    | Call Label
    | Xchg CGMem CGMem
    | Cmp CGMem CGMem
    | Jmp Label 
    | Jg Label
    | Jl Label
    | Jle Label 
    | Jge Label
    | Jeq Label
    | Jne Label
    | Push CGMem
    | Pop CGMem
    | Section String
    | Str String
    | Leave
    | Ret
    | Global Label
    | Lbl Label
    | P2Align
    | Function Label
    deriving (Eq, Ord)

instance Show AsmInstr where
    show i = case i of  
        Mov dst src    -> "    movq " ++ show src ++ ", " ++ show dst
        Lea dst src    -> "    leaq " ++ show src ++ ", " ++ show dst
        Add dst src    -> "    addq " ++ show src ++ ", " ++ show dst
        Sub dst src    -> "    subq " ++ show src ++ ", " ++ show dst
        Imul dst src   -> "    imulq " ++ show src ++ ", " ++ show dst
        Idiv reg       -> "    idivq " ++ show reg
        Cqo            -> "    cqo"
        Call lbl       -> "    call " ++ lbl
        Xchg larg rarg -> "    xchgq " ++ show rarg ++ ", " ++ show larg
        Cmp larg rarg  -> "    cmpq " ++ show rarg ++ ", " ++ show larg
        Jmp lbl        -> "    jmp " ++ lbl
        Jg lbl         -> "    jg " ++ lbl
        Jl lbl         -> "    jl " ++ lbl
        Jle lbl        -> "    jle " ++ lbl
        Jge lbl        -> "    jge " ++ lbl
        Jeq lbl        -> "    je " ++ lbl
        Jne lbl        -> "    jne " ++ lbl
        Push mem       -> "    pushq " ++ show mem
        Pop mem        -> "    popq " ++ show mem
        Section str    -> ".section " ++ str
        Str str        -> "    .string " ++ str
        Leave          -> "    leave"
        Ret            -> "    ret"
        Global lbl     -> "    .globl " ++ lbl
        Lbl lbl        -> lbl ++ ":"
        P2Align        -> "    .p2align 4,,15"
        Function name  -> "    .type " ++ name ++ ", @function"
        

data CGMachineState = CGMS {
    regToVar :: Map CGReg IRAddr,
    varToMem :: Map IRAddr CGMem,
    nextInstrs :: [(IRInstr, LiveMap)],
    generatedCode :: [AsmInstr],
    currentStackPos :: Integer
}

data CGState = CGS {
    blocks :: Map Label LiveBlock,
    liveIn :: Map Label (Set IRAddr),
    strToLbl :: Map String Label,
    nextStrLbl :: Integer,
    nextLbl :: Integer,
    currentMs :: CGMachineState,
    blockToMs :: Map Label CGMachineState,
    blockToInitialMs :: Map Label CGMachineState,
    nextStackLoc :: Integer,
    nonvolatileRegsUsed :: Set CGReg,
    blToNru :: Map Label [CGReg]
}

type CGMonad = State CGState
