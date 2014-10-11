module LinearMatrixCode(
  LinearMatrixCode, Result,
  linBinop, linUnop,
  result, allocateMatrix) where

import DataObject
import ImperativeCode

type Result = DataObject
type Operand = DataObject

data LinearMatrixCode
  = MatBinop String Result Operand Operand
  | MatUnop String Result Operand
  | AllocateMatrix Result
    deriving (Eq, Ord, Show)

result (MatBinop _ r _ _) = r
result (MatUnop _ r _) = r

allocateMatrix = AllocateMatrix
linBinop = MatBinop
linUnop = MatUnop

toImperativeCode :: [LinearMatrixCode] -> ImperativeProgram
toImperativeCode [] = []
