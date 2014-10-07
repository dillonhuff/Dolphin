module DataObject(
  DataObject,
  dataName, mDim, nDim, matrix,
  scalar, rowVector, colVector) where

type Name = String

data DataObject = DataObject {
  dataName :: Name,
  mDim :: Dimension,
  nDim :: Dimension
  } deriving (Eq, Ord, Show)

data Dimension
  = One
  | Symbolic Name
    deriving (Eq, Ord, Show)

matrix name = DataObject name (Symbolic (name ++ "_num_rows")) (Symbolic (name ++ "_num_cols"))
rowVector name = DataObject name One $ Symbolic (name ++ "_num_cols")
colVector name = DataObject name (Symbolic (name ++ "_num_rows")) One
scalar name = DataObject name One One
