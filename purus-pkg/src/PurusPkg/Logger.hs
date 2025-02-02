{- | Module: PurusPkg.Logger

Logging utilities for the package manager


TODO(jaredponn) February 4, 2025: this is all very basic simple logging
-}
module PurusPkg.Logger (logInfo, logWarn) where

import System.IO qualified as IO

logInfo :: String -> IO ()
logInfo = IO.hPutStrLn IO.stderr

logWarn :: String -> IO ()
logWarn = IO.hPutStrLn IO.stderr
