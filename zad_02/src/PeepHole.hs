module PeepHole where

import Data.Maybe

import CGDef

-- optimizeTwoInstructions :: [AsmInstr]

optimizeSingleInstruction :: AsmInstr -> Maybe AsmInstr
optimizeSingleInstruction i = case i of
    Add _ (Lit (CGInt 0)) -> Nothing
    Sub _ (Lit (CGInt 0)) -> Nothing
    Imul _ (Lit (CGInt 1)) -> Nothing
    _ -> Just i

-- optimizeDouble :: [AsmInstr] -> [AsmInstr]

optimizeSingle :: [AsmInstr] -> [AsmInstr]
optimizeSingle = mapMaybe optimizeSingleInstruction

optimizePeepHole :: [AsmInstr] -> [AsmInstr]
optimizePeepHole = optimizeSingle
