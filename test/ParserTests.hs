module ParserTests(allParserTests) where

import DataObject
import Parser
import TestUtils

allParserTests = do
  testFunction parseComputation parseCases

parseCases =
  [("r_y <- r_x + r_y",
    assign (rowVector "y") (matrixAdd (matrixData (rowVector "x")) (matrixData (rowVector "y"))))]
