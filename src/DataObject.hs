module DataObject(
  DataObject,
  dataObject,
  resDataObjectUnop, resDataObjectBinop,
  dataName, mDim, nDim, matrix,
  scalar, rowVector, colVector,
  symbolicDim,) where

type Name = String

data DataObject = DataObject {
  dataName :: Name,
  mDim :: Dimension,
  nDim :: Dimension
  } deriving (Eq, Ord, Show)

dataObject = DataObject

data Dimension
  = One
  | Symbolic Name
    deriving (Eq, Ord, Show)

symbolicDim = Symbolic

matrix name = DataObject name (Symbolic (name ++ "_num_rows")) (Symbolic (name ++ "_num_cols"))
rowVector name = DataObject name One $ Symbolic (name ++ "_num_cols")
colVector name = DataObject name (Symbolic (name ++ "_num_rows")) One
scalar name = DataObject name One One

resDataObjectBinop :: String -> String -> DataObject -> DataObject -> DataObject
resDataObjectBinop resultName "*" left right =
  DataObject resultName (mDim left) (nDim right)
resDataObjectBinop resultName _ left right =
  DataObject resultName (mDim right) (nDim right)

resDataObjectUnop :: String -> String -> DataObject -> DataObject
resDataObjectUnop resultName "'" op =
  DataObject resultName (nDim op) (mDim op)
resDataObjectUnop resultName _ op =
  DataObject resultName (mDim op) (nDim op)
