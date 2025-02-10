{- Module: PurusPkg.LocalRegistry.Cli

Data types and functionality for the CLI interface.

 -}
module PurusPkg.LocalRegistry.Cli (Options (..), Command (..), BuildOptions (..), getOptions) where

import Options.Applicative (Parser)
import Options.Applicative qualified as Applicative

newtype Options = Options {oCommand :: Command}

data Command
  = CommandBuild BuildOptions

data BuildOptions = BuildOptions
  { boPackagePaths :: [FilePath]
  }

-- | Gets the CLI options provided
getOptions :: IO Options
getOptions = Applicative.execParser $ Applicative.info (pOptions Applicative.<**> Applicative.helper) Applicative.idm

pOptions :: Parser Options
pOptions =
  Options <$> pCommand

pCommand :: Parser Command
pCommand =
  Applicative.hsubparser
    ( Applicative.command "build" (Applicative.info (CommandBuild <$> pBuild) (Applicative.progDesc $ "Builds a local registry and dumps it to STDOUT"))
    )

pBuild :: Parser BuildOptions
pBuild = BuildOptions <$> Applicative.many pPackagePath

pPackagePath :: Parser FilePath
pPackagePath =
  Applicative.strOption
    ( Applicative.long "purus-package-path"
        <> Applicative.metavar "PATH"
        <> Applicative.help
          "Path to a Purus package"
    )
