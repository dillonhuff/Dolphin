module ImperativeCode(
  ImperativeProgram) where

import DataObject

type ImperativeProgram = [Statement]

data Statement
  = Loop { var :: IndexVar, start :: IndexExpr, end :: IndexExpr, inc :: IndexExpr }
  | Assign DataRef Update
  | Allocate DataObject
    deriving (Eq, Ord, Show)

data Update
  = Ref DataRef
  | Binop String Update Update
    deriving (Eq, Ord, Show)

data DataRef
  = DataRef { object :: DataObject, index :: IndexExpr }
    deriving (Eq, Ord, Show)

data IndexExpr
  = IndSingle IndexBase
  | IndTimes IndexBase IndexBase
  | IndSum IndexExpr IndexExpr
    deriving (Eq, Ord, Show)

data IndexBase
  = Const Int
  | Var IndexVar
    deriving (Eq, Ord, Show)

data IndexVar = IndexVar String
                deriving (Eq, Ord, Show)
