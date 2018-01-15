module CodeGen where

import Data.Map as M

import IRDef
import Live

data CGReg = RDI | RSI | RDX | RCX | R8 | R9 | RAX | R10 | R11
            | RBX | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord)

data CGMem = Reg CGReg | Mem CGReg Integer

data CGMachineState = CGMS {
    regToVar :: Map CGReg IRAddr,
    varToMem :: Map IRAddr CGMem
}

-- data CGState = CGS {

-- }

-- prologue :: [String] -> [String]
-- prologue = [
--     ""
-- ]


-- collectStringsFromBlocks :: [Label] -> 

generateAsm :: LiveState -> [Label] -> [String]
generateAsm (LS _ li extb) blockOrder = do
    ["hello"]
