module ImperativeCode(
  ImperativeProgram,
  Statement,
  loop, assign,
  allocateMemory,
  freeMemory,
  ref, binop, unop,
  symAccess, unitInc,
  indexConstExpr, indexVarExpr) where

import DataObject

type ImperativeProgram = [Statement]

data Statement
  = Loop { var :: IndexVar, start :: IndexExpr, end :: IndexExpr, inc :: IndexExpr, body :: [Statement] }
  | Assign DataRef Update
  | Allocate DataObject
  | Free DataObject
    deriving (Eq, Ord, Show)

loop :: String -> IndexExpr -> IndexExpr -> IndexExpr -> [Statement] -> Statement
loop varName startExpr endExpr increment statements =
  Loop (IndexVar varName) startExpr endExpr increment statements

assign = Assign
allocateMemory = Allocate
freeMemory = Free

data Update
  = Ref DataRef
  | Unop String Update
  | Binop String Update Update
    deriving (Eq, Ord, Show)

ref = Ref
binop = Binop
unop = Unop

symAccess :: DataObject -> String -> String -> DataRef
symAccess obj rowVar colVar = DataRef obj indRef
  where
    rowLoc = IndTimes (Var $ IndexVar rowVar) (Var $ IndexVar $ rowStride obj)
    colLoc = IndTimes (Var $ IndexVar colVar) (Var $ IndexVar $ colStride obj)
    indRef = IndSum rowLoc colLoc

data DataRef
  = DataRef { object :: DataObject, index :: IndexExpr }
    deriving (Eq, Ord, Show)

data IndexExpr
  = IndSingle IndexBase
  | IndTimes IndexBase IndexBase
  | IndSum IndexExpr IndexExpr
    deriving (Eq, Ord, Show)

unitInc varName = IndSum (indexVarExpr varName) (indexConstExpr 1)
indexConstExpr = IndSingle . Const
indexVarExpr = IndSingle . Var . IndexVar

data IndexBase
  = Const Int
  | Var IndexVar
    deriving (Eq, Ord, Show)

data IndexVar = IndexVar String
                deriving (Eq, Ord, Show)
