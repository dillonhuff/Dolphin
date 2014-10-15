module ImperativeCode(
  ImperativeProgram,
  Statement,
  prettyC,
  loop, assign,
  allocateMemory,
  freeMemory,
  ref, binop, unop,
  symAccess, unitInc,
  indexConstExpr, indexVarExpr) where

import DataObject

prettyC :: ImperativeProgram -> String
prettyC program = concat $ map (printC 0) program

type ImperativeProgram = [Statement]

printC :: Int -> Statement -> String
printC indentLevel (Loop v s e i b) =
  indent indentLevel ++ loopHeader ++ loopBody ++ loopEnd
  where
    startCond = varToC v ++ " = " ++ indexExprToC s
    endCond = varToC v ++ " < " ++ indexExprToC e
    update = varToC v ++ " = " ++ indexExprToC i
    loopCond = startCond ++ "; " ++ endCond ++ "; " ++ update
    loopHeader = "for ( " ++ loopCond ++ " ) {\n"
    loopBody = concat $ map (printC (indentLevel + 1)) b
    loopEnd = indent indentLevel ++ "}\n"
printC indentLevel (Assign dRef update) = indent indentLevel ++ asgStr ++ ";\n"
  where
    dRefStr = refToC dRef
    updateStr = updateToC update
    asgStr = dRefStr ++ " = " ++ updateStr
printC indentLevel (Allocate obj) = indent indentLevel ++ allocStr
  where
    allocSize = "sizeof( double ) * " ++ (mDimName obj) ++ " * " ++ (nDimName obj)
    allocStr = "double* " ++ dataName obj ++ " = malloc( " ++ allocSize ++ " );\n"
printC indentLevel (Free obj) = indent indentLevel ++ freeStr
  where
    freeStr = "free( " ++ dataName obj ++ " );\n"

indent :: Int -> String
indent level = replicate level '\t'

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

updateToC :: Update -> String
updateToC (Ref dRef) = refToC dRef
updateToC (Unop name op) = name ++ updateToC op
updateToC (Binop name l r) = updateToC l ++ " " ++ name ++ " " ++ updateToC r

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

refToC :: DataRef -> String
refToC (DataRef obj indexExpr) = dataName obj ++ "[ " ++ indexExprToC indexExpr ++ " ]"

data IndexExpr
  = IndSingle IndexBase
  | IndTimes IndexBase IndexBase
  | IndSum IndexExpr IndexExpr
    deriving (Eq, Ord, Show)

indexExprToC :: IndexExpr -> String
indexExprToC (IndSingle base) = baseToC base
indexExprToC (IndTimes l r) = baseToC l ++ " * " ++ baseToC r
indexExprToC (IndSum l r) = indexExprToC l ++ " + " ++ indexExprToC r

unitInc varName = IndSum (indexVarExpr varName) (indexConstExpr 1)
indexConstExpr = IndSingle . Const
indexVarExpr = IndSingle . Var . IndexVar

data IndexBase
  = Const Int
  | Var IndexVar
    deriving (Eq, Ord, Show)

baseToC :: IndexBase -> String
baseToC (Const val) = show val
baseToC (Var var) = varToC var

data IndexVar = IndexVar String
                deriving (Eq, Ord, Show)

varToC :: IndexVar -> String
varToC (IndexVar name) = name

