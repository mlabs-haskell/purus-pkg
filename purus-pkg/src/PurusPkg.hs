{- | Module: PurusPkg

Exposes the main function to run the whole PurusPkg show.
-}
module PurusPkg where

import PurusPkg.Cli qualified
import PurusPkg.Registries qualified
import PurusPkg.Solver qualified

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.Traversable qualified as Traversable

import System.Process qualified as Process

main :: IO ()
main =
  PurusPkg.Cli.getOptions >>= \options -> case PurusPkg.Cli.oCommand options of
    PurusPkg.Cli.CommandBuild {} ->
      Process.callProcess "purus" [PurusPkg.Solver.purusModulesDirectory]
    PurusPkg.Cli.CommandCheck {} ->
      Process.callProcess "purs" ["compile", "**/*.purs"]
    PurusPkg.Cli.CommandInstall installOptions -> do
      -- set up the registries
      let registryOptions = PurusPkg.Cli.ioRegistryOptions installOptions
      registries <- registryOptionsToRegistries registryOptions

      -- get the current package.json
      currentPackage <- do
        eitherResult <- Aeson.eitherDecodeFileStrict' PurusPkg.Registries.packageFilePath
        either (Exception.throwIO . Aeson.AesonException) return eitherResult

      -- run the solver to get the dependencies
      dependencies <- PurusPkg.Solver.solver currentPackage registries

      -- create the directory of dependencies
      PurusPkg.Solver.createPurusModules dependencies registries

{- | Converts the options of registries to 'PurusPkg.Registries.Registries'
i.e., the form of a registry that can actually be used to query the sources
from an associated name and version
-}
registryOptionsToRegistries :: [PurusPkg.Cli.RegistryOption] -> IO PurusPkg.Registries.Registries
registryOptionsToRegistries registryOptions = do
  registries <- Traversable.for registryOptions $ \registryOption ->
    case registryOption of
      PurusPkg.Cli.LocalRegistryOption filepath -> do
        eitherResult <- Aeson.eitherDecodeFileStrict' filepath
        localRegistryMapping <- either (Exception.throwIO . Aeson.AesonException) return eitherResult
        return PurusPkg.Registries.LocalRegistry {PurusPkg.Registries.lrFilePath = filepath, PurusPkg.Registries.lrMapping = localRegistryMapping}

  return $ PurusPkg.Registries.Registries $ reverse registries
