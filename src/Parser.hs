module Parser(
  parseComputation,
  matrixAdd, matrixData, matrixMultiply,
  matrixSubtract, scalarMultiply,
  matrixTranspose, matrixNegate,
  assign, toLinearMatrixCode) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim

import DataObject
import LinearMatrixCode

toLinearMatrixCode :: LAComputation -> [LinearMatrixCode]
toLinearMatrixCode (Assign res (MatrixData obj)) = [linUnop "copy" res obj]
toLinearMatrixCode (Assign res expr) = exprCode ++ [linUnop "copy" res result]
  where
    (exprCode, result) = linearizeMatrixOperationTree expr "#0"

linearizeMatrixOperationTree :: MatrixOperationParseTree ->
                                String ->
                                ([LinearMatrixCode], Result)
linearizeMatrixOperationTree (MatrixBinop name left right) suffix =
  (leftCode ++ rightCode ++ [allocateMatrix resultTmp, linBinop name resultTmp leftRes rightRes], resultTmp)
  where
    resName = newTemp suffix
    (leftCode, leftRes) = linearizeMatrixOperationTree left (suffix ++ "1")
    (rightCode, rightRes) = linearizeMatrixOperationTree right (suffix ++ "2")
    resultTmp = resDataObjectBinop resName name leftRes rightRes
linearizeMatrixOperationTree (MatrixUnop name operand) suffix =
  (operandCode ++ [allocateMatrix resultTmp, linUnop name resultTmp operandRes], resultTmp)
  where
    resName = newTemp suffix
    (operandCode, operandRes) = linearizeMatrixOperationTree operand (suffix ++ "0")
    resultTmp = resDataObjectUnop resName name operandRes
linearizeMatrixOperationTree (MatrixData obj) suffix = ([], obj)

newTemp suffix = "T" ++ suffix

data LAComputation
  = Assign DataObject MatrixOperationParseTree
    deriving (Eq, Ord, Show)

assign = Assign

data MatrixOperationParseTree
  = MatrixBinop String MatrixOperationParseTree MatrixOperationParseTree
  | MatrixUnop String MatrixOperationParseTree
  | MatrixData DataObject
    deriving (Eq, Ord, Show)

matrixData = MatrixData
matrixAdd = MatrixBinop "+"
matrixSubtract = MatrixBinop "-"
scalarMultiply = MatrixBinop ".*"
matrixMultiply = MatrixBinop "*"
matrixTranspose = MatrixUnop "'"
matrixNegate = MatrixUnop "-"

parseComputation :: String -> LAComputation
parseComputation text = case parse pLAComputation "Computation Parser" toks of
  Left err -> error $ show err
  Right res -> res
  where
    toks = lexComputation text

pLAComputation = do
  updated <- dataObjTok
  assignTok
  res <- pExpr
  return $ Assign (getData updated) res

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
  dolphinOp opName
  return $ MatrixBinop opName

unaryOp opName = do
  dolphinOp opName
  return $ MatrixUnop opName
  
pTerm = pParens pExpr
        <|> pMatrixData

pMatrixData :: (Monad m) => ParsecT [Token] u m MatrixOperationParseTree
pMatrixData = do
  obj <- dataObjTok
  return $ MatrixData $ getData obj

pParens e = do
  lParenTok
  res <- e
  rParenTok
  return res

dataObjTok :: (Monad m) => ParsecT [Token] u m Token
dataObjTok = dolphinTok isDataObj
  where
    isDataObj x = case x of
      (DataTok n _) -> True
      _ -> False

lParenTok :: (Monad m) => ParsecT [Token] u m Token
lParenTok = dolphinTok isLParen

rParenTok :: (Monad m) => ParsecT [Token] u m Token
rParenTok = dolphinTok isRParen

dolphinOp :: (Monad m) => String -> ParsecT [Token] u m Token
dolphinOp name = dolphinTok (\t -> isOpTok t && getOpName t == name)

assignTok :: (Monad m) => ParsecT [Token] u m Token
assignTok = dolphinTok isAssign

dolphinTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
dolphinTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position

-- Lexer
data Token
  = DataTok DataObject SourcePos
  | AsgTok SourcePos
  | OpTok String SourcePos
  | DelimTok String SourcePos
    deriving (Eq, Ord, Show)

getData (DataTok d _) = d
getOpName (OpTok n _) = n

isAssign (AsgTok _) = True
isAssign _ = False

isOpTok (OpTok _ _) = True
isOpTok _ = False

isLParen (DelimTok "(" _) = True
isLParen _ = False

isRParen (DelimTok ")" _) = True
isRParen _ = False

pos :: Token -> SourcePos
pos (DataTok _ p) = p
pos (AsgTok p) = p
pos (OpTok _ p) = p
pos (DelimTok _ p) = p

lexComputation :: String -> [Token]
lexComputation text = case parse (sepBy dolphinToken spaces) "Lexer" text of
  Left err -> error $ show err
  Right dolphinTokens -> dolphinTokens

dolphinToken = pDataObject <|> pAsg <|> pOperation <|> pDelimiter

pDataObject = do
  pos <- getPosition
  obj <- pMatrix <|> pVectorOrScalar
  return $ DataTok obj pos

pAsg = do
  pos <- getPosition
  string "<-"
  return $ AsgTok pos

pOperation = do
  pos <- getPosition
  op <- string "*"
        <|> string ".*"
        <|> string "+"
        <|> string "-"
        <|> string "'"
  return $ OpTok op pos

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

pDelimiter = do
  pos <- getPosition
  delimiter <- string "(" <|> string ")"
  return $ DelimTok delimiter pos
