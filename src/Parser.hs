module Parser(
  parseComputation,
  matrixAdd, matrixData, matrixMultiply,
  matrixTranspose,
  assign) where

import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Prim

import DataObject

data LAComputation
  = Assign DataObject MatrixOperationParseTree
    deriving (Eq, Ord, Show)

assign = Assign

data MatrixOperationParseTree
  = MatrixBinop String MatrixOperationParseTree MatrixOperationParseTree
  | MatrixUnop String MatrixOperationParseTree
  | MatrixData DataObject
    deriving (Eq, Ord, Show)

matrixAdd = MatrixBinop "+"
matrixMultiply = MatrixBinop "*"
matrixTranspose = MatrixUnop "'"
matrixData = MatrixData

parseComputation :: String -> LAComputation
parseComputation text = case parse pLAComputation "Computation Parser" text of
  Left err -> error $ show err
  Right c -> c

pLAComputation = do
  updated <- pDataObject
  spaces
  string "<-"
  spaces
  newVal <- pExpr
  return $ Assign updated newVal

pExpr = buildExpressionParser table pTerm

table =
  [[negation, transpose],
   [matrixMultiplication, scalarMultiplication],
   [addition, subtraction]]
  
negation = Prefix (unaryOp "-")
transpose = Postfix (unaryOp "'")
addition = Infix (binaryOp "+") AssocLeft
subtraction = Infix (binaryOp "-") AssocLeft
matrixMultiplication = Infix (binaryOp "*") AssocLeft
scalarMultiplication = Infix (binaryOp ".*") AssocLeft

binaryOp opName = do
  string opName
  return $ MatrixBinop opName

unaryOp opName = do
  string opName
  return $ MatrixUnop opName
  
pTerm = pMatrixData

pMatrixData = do
  spaces
  obj <- pDataObject
  spaces
  return $ matrixData obj

pParens e = do
  char '('
  spaces
  res <- e
  spaces
  char ')'
  return e

pDataObject = pMatrix <|> pVectorOrScalar

pMatrix = do
  first <- upper
  rest <- many alphaNum
  return $ matrix (first:rest)
  
pVectorOrScalar = do
  orientation <- oneOf "rcs"
  char '_'
  first <- lower
  rest <- many alphaNum
  case orientation of
    's' -> return $ scalar (first:rest)
    'r' -> return $ rowVector (first:rest)
    'c' -> return $ colVector (first:rest)
