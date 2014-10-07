module ParserTests(allParserTests) where

import DataObject
import Parser
import TestUtils

allParserTests = do
  testFunction parseComputation parseCases

parseCases =
  [("r_y <- r_x + r_y",
    assign (rowVector "y") (matrixAdd (matrixData (rowVector "x")) (matrixData (rowVector "y")))),
   ("c_z <- r_x * A' * c_y", assign (colVector "z") (matrixMultiply (matrixMultiply (matrixData (rowVector "x")) (matrixTranspose (matrixData (matrix "A")))) (matrixData (colVector "y"))))]
