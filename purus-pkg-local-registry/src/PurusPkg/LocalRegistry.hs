{- Module: PurusPkg.LocalRegistry

Module to define the main function of this package
 -}
module PurusPkg.LocalRegistry where

import PurusPkg.Package qualified
import PurusPkg.Registries qualified

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.Map qualified as Map
import Data.Traversable qualified as Traversable
import PurusPkg.LocalRegistry.Cli qualified
import System.FilePath qualified as FilePath

main :: IO ()
main = do
  options <- PurusPkg.LocalRegistry.Cli.getOptions
  case PurusPkg.LocalRegistry.Cli.oCommand options of
    PurusPkg.LocalRegistry.Cli.CommandBuild buildOptions -> do
      localRegistryMapping <- makeLocalRegistryMapping $ PurusPkg.LocalRegistry.Cli.boPackagePaths buildOptions
      ByteString.Lazy.Char8.putStrLn $ Aeson.encode localRegistryMapping

makeLocalRegistryMapping :: [FilePath] -> IO PurusPkg.Registries.LocalRegistryMapping
makeLocalRegistryMapping filepaths =
  fmap (PurusPkg.Registries.LocalRegistryMapping . Map.fromList) $ Traversable.for filepaths $ \path -> do
    let packageJson = path FilePath.</> PurusPkg.Registries.packageFilePath
    eitherResult <- Aeson.eitherDecodeFileStrict packageJson
    case eitherResult of
      Left err -> Exception.throwIO $ Aeson.AesonException err
      Right package ->
        return
          ( (PurusPkg.Package.pName package, PurusPkg.Package.pVersion package)
          , PurusPkg.Registries.FileReference path
          )
