module LinearMatrixCode(
  LinearMatrixCode, Result,
  linBinop, linUnop,
  result) where

import DataObject

type Result = DataObject
type Operand = DataObject

data LinearMatrixCode
  = MatBinop String Result Operand Operand
  | MatUnop String Result Operand
    deriving (Eq, Ord, Show)

result (MatBinop _ r _ _) = r
result (MatUnop _ r _) = r


linBinop = MatBinop
linUnop = MatUnop
