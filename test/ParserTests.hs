module ParserTests(allParserTests) where

import DataObject
import LinearMatrixCode
import Parser
import TestUtils

allParserTests = do
  testFunction parseComputation parseCases
  testFunction (toLinearMatrixCode . parseComputation) linMatCodeCases

parseCases =
  [("r_y <- r_x + r_y",
    assign (rowVector "y") (matrixAdd (matrixData (rowVector "x")) (matrixData (rowVector "y")))),
   ("c_z <- r_x * (A   ') * c_y", assign (colVector "z") (matrixMultiply (matrixMultiply (matrixData (rowVector "x")) (matrixTranspose (matrixData (matrix "A")))) (matrixData (colVector "y")))),
   ("C <- s_alpha .* (A'*B) - (s_beta.*-C)",
    assign (matrix "C") (matrixSubtract (scalarMultiply (matrixData $ scalar "alpha") (matrixMultiply (matrixTranspose (matrixData $ matrix "A")) (matrixData $ matrix "B"))) (scalarMultiply (matrixData $ scalar "beta") (matrixNegate (matrixData $ matrix "C")))))]

linMatCodeCases =
  [("r_y <- r_z", [linUnop "copy" (rowVector "y") (rowVector "z")]),
   ("A <- B * C",
    let tmp = dataObject "T_0" (symbolicDim "B_num_rows") (symbolicDim "C_num_cols") in
    [allocateMatrix tmp, linBinop "*" tmp (matrix "B") (matrix "C"), linUnop "copy" (matrix "A") tmp]),
   ("A <- C'",
    let tmp = dataObject "T_0" (symbolicDim "C_num_cols") (symbolicDim "C_num_rows") in
    [allocateMatrix tmp, linUnop "'" tmp (matrix "C"), linUnop "copy" (matrix "A") tmp])]
