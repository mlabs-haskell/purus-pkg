module PurusPkg.Solver.Test where

import Test.Tasty (TestTree)
import Test.Tasty qualified

tests :: TestTree
tests =
  Test.Tasty.testGroup
    "Solver tests"
    []

-- TODO(jaredponn) February 5, 2025: with the way its written now,
-- it's kinda hard to do this
