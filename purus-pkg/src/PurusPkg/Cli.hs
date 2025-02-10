{-# LANGUAGE OverloadedStrings #-}

{- | Module: PurusPkg.Cli

Exposes the types, and functions for the CLI interface
-}
module PurusPkg.Cli where

import Options.Applicative (Parser)
import Options.Applicative qualified as Applicative

import PurusPkg.Package qualified as Package
import PurusPkg.Registries (LocalRegistryMapping (LocalRegistryMapping), LocalRegistryReference (FileReference))
import PurusPkg.Solver (purusModulesDirectory)

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Map qualified as Map

newtype Options = Options {oCommand :: Command}

data Command
  = CommandInstall InstallOptions
  | CommandBuild
  | CommandCheck

data InstallOptions = InstallOptions
  { ioRegistryOptions :: [RegistryOption]
  }

data RegistryOption
  = LocalRegistryOption FilePath

-- | Gets the CLI options provided
getOptions :: IO Options
getOptions = Applicative.execParser $ Applicative.info (pOptions Applicative.<**> Applicative.helper) Applicative.idm

pOptions :: Parser Options
pOptions =
  Options <$> pCommand

pCommand :: Parser Command
pCommand =
  Applicative.hsubparser
    ( Applicative.command "install" (Applicative.info (CommandInstall <$> pInstall) (Applicative.progDesc $ "Install the dependencies to " <> purusModulesDirectory))
        <> Applicative.command "build" (Applicative.info (pure CommandBuild) (Applicative.progDesc $ "Run the purus compiler"))
        <> Applicative.command "check" (Applicative.info (pure CommandCheck) (Applicative.progDesc $ "Check the source files typecheck (generates corefn as well)"))
    )

pInstall :: Parser InstallOptions
pInstall = InstallOptions <$> Applicative.many pRegistryOption

pRegistryOption :: Parser RegistryOption
pRegistryOption =
  fmap LocalRegistryOption $
    Applicative.strOption
      ( Applicative.long "local-registry"
          <> Applicative.metavar "FILE"
          <> Applicative.help
            ( let eg =
                    ByteString.Char8.unpack $
                      ByteString.toStrict $
                        Aeson.encode $
                          LocalRegistryMapping $
                            Map.fromList [(("my-package", either error id $ Package.versionFromText "1.0.0"), FileReference "./path/to/my-package")]
               in "A file path to a JSON encoded `PurusPkg.Registries.Registry` "
                    ++ "(e.g. "
                    ++ eg
                    ++ ")"
                    ++ " to lookup the associated sources given a name and version. In the case of multiple registries providing sources for the same name and version, the rightmost registry takes priority."
            )
      )
