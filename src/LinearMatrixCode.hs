module LinearMatrixCode(
  LinearMatrixCode, Result,
  linBinop, linUnop,
  result, allocateMatrix,
  toImperativeCode) where

import Data.List as L
import Data.Set as S

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

dataObjects :: LinearMatrixCode -> Set DataObject
dataObjects (MatBinop _ res l r) = S.fromList [res, l , r]
dataObjects (MatUnop _ res op) = S.fromList [res, op]
dataObjects _ = S.empty

allocateMatrix = AllocateMatrix
linBinop = MatBinop
linUnop = MatUnop

toImperativeCode :: [LinearMatrixCode] -> ImperativeProgram
toImperativeCode code = L.map toImperativeStatement preprocessedCode
  where
    preprocessedCode = addDeallocates code

addDeallocates :: [LinearMatrixCode] -> [LinearMatrixCode]
addDeallocates code = reverse $ addDeallocs allTemps $ reverse code
  where
    allAllocs = L.filter isAlloc code
    allTemps = S.fromList $ L.map (\ (AllocateMatrix obj) -> obj) allAllocs

addDeallocs :: Set DataObject -> [LinearMatrixCode] -> [LinearMatrixCode]
addDeallocs _ [] = []
addDeallocs notFreedYet (st:sts) = toFree ++ (st : (addDeallocs stillUnFreed sts))
  where
    lastUseInSt = S.intersection notFreedYet (dataObjects st)
    toFree = S.toList $ S.map FreeMatrix lastUseInSt
    stillUnFreed = S.difference notFreedYet lastUseInSt

isAlloc :: LinearMatrixCode -> Bool
isAlloc (AllocateMatrix m) = True
isAlloc _ = False

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
