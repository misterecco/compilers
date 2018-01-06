module IRDef where

import Data.List ( intercalate )

type Label = String
type Name = String

data IRVar = IRVar Name Integer IRType
    deriving (Eq, Ord)

instance Show IRVar where
    show (IRVar name int irtype) = show irtype ++ " " ++ name ++ " " ++ show int

data IRType = IRInt | IRStr | IRBool | IRVoid
    deriving (Eq, Ord)

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
    deriving (Eq, Ord)

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

showArgs :: Show a => [a] -> String
showArgs args = intercalate ", " (map show args)

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

addrType :: IRAddr -> IRType
addrType (ImmInt _) = IRInt
addrType (ImmString _) = IRStr
addrType (ImmBool _) = IRBool
addrType NoRet = IRVoid
addrType (Indirect (IRVar _ _ t)) = t
