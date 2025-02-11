module Main where

import Prelude

-- `main` will be the script that is compiled to UPLC
main :: Int -> Int -> Builtin.BuiltinData -> Builtin.BuiltinData -> Boolean 
main a b _scriptContext _redeemer = a + b == 3

