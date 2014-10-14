module LinearMatrixCode(
  LinearMatrixCode, Result,
  linBinop, linUnop,
  result, allocateMatrix,
  toImperativeCode) where

import DataObject
import ImperativeCode

type Result = DataObject
type Operand = DataObject

data LinearMatrixCode
  = MatBinop String Result Operand Operand
  | MatUnop String Result Operand
  | AllocateMatrix Operand
  | FreeMatrix Operand
    deriving (Eq, Ord, Show)

result (MatBinop _ r _ _) = r
result (MatUnop _ r _) = r

allocateMatrix = AllocateMatrix
linBinop = MatBinop
linUnop = MatUnop

toImperativeCode :: [LinearMatrixCode] -> ImperativeProgram
toImperativeCode code = map toImperativeStatement preprocessedCode
  where
    preprocessedCode = addDeallocates code

addDeallocates :: [LinearMatrixCode] -> [LinearMatrixCode]
addDeallocates c = c

toImperativeStatement :: LinearMatrixCode -> Statement
toImperativeStatement (AllocateMatrix m) = allocateMemory m
toImperativeStatement (FreeMatrix m) = freeMemory m
toImperativeStatement (MatUnop "'" res op) =
  loop "i" (indexConstExpr 0) (indexVarExpr (nDimName res)) (unitInc "i") $
  [loop "j" (indexConstExpr 0) (indexVarExpr (mDimName res)) (unitInc "j") $
  [assign 
  (symAccess res "i" "j") (ref $ symAccess op "j" "i")]]
toImperativeStatement (MatUnop "copy" res op) =
  loop "i" (indexConstExpr 0) (indexVarExpr (nDimName res)) (unitInc "i") $
  [loop "j" (indexConstExpr 0) (indexVarExpr (mDimName res)) (unitInc "j") $
  [assign 
  (symAccess res "i" "j") (ref $ symAccess op "i" "j")]]
toImperativeStatement (MatUnop "-" res op) =
  loop "i" (indexConstExpr 0) (indexVarExpr (nDimName res)) (unitInc "i") $
  [loop "j" (indexConstExpr 0) (indexVarExpr (mDimName res)) (unitInc "j") $
  [assign 
  (symAccess res "i" "j")
  (unop "-" (ref $ symAccess op "i" "j"))]]
toImperativeStatement (MatBinop "*" res l r) =
  loop "j" (indexConstExpr 0) (indexVarExpr (nDimName r)) (unitInc "j") $
  [loop "i" (indexConstExpr 0) (indexVarExpr (mDimName l)) (unitInc "i") $
  [loop "k" (indexConstExpr 0) (indexVarExpr (mDimName r)) (unitInc "k") $
  [assign 
  (symAccess res "i" "j")
  (binop "+" (ref $ symAccess res "i" "j")
   (binop "*" (ref $ symAccess l "i" "k") (ref $ symAccess r "j" "k")))]]]
toImperativeStatement (MatBinop n res l r) =
  loop "i" (indexConstExpr 0) (indexVarExpr (nDimName l)) (unitInc "i") $
  [loop "j" (indexConstExpr 0) (indexVarExpr (mDimName l)) (unitInc "j") $
  [assign 
  (symAccess res "i" "j")
  (binop n (ref $ symAccess l "i" "j") (ref $ symAccess r "i" "j"))]]
