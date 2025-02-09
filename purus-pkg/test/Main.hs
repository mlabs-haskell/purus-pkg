module Main (main) where

import Test.Tasty qualified
import Test.Tasty.QuickCheck qualified

import PurusPkg.Package.Test qualified
import PurusPkg.Solver.Test qualified

main :: IO ()
main =
  Test.Tasty.defaultMain $
    Test.Tasty.adjustOption (\(Test.Tasty.QuickCheck.QuickCheckTests numberOfTests) -> Test.Tasty.QuickCheck.QuickCheckTests $ max numberOfTests 10000) $
      Test.Tasty.testGroup
        "purus-pkg tests"
        [ PurusPkg.Solver.Test.tests
        , PurusPkg.Package.Test.tests
        ]
