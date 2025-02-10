{- Module: PurusPkg.Registries

Data types and functionality to build a stack of registries i.e., a symbol
table to lookup package sources associated with a name and a version.

 -}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module PurusPkg.Registries (
  Registries (..),
  Registry (..),
  queryRegistriesPackage,
  queryRegistriesPackageSources,
  queryRegistriesSatisfyingVersions,
  LocalRegistryMapping (..),
  LocalRegistryReference (..),
  packageFilePath,
)
where

import Data.Map (Map)
import Data.Map qualified as Map

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import System.FilePath qualified as FilePath
import System.Process qualified as Process

import Data.Aeson qualified as Aeson
import Data.Maybe qualified as Maybe
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Data.SemVer qualified as SemVer
import PurusPkg.Logger qualified as Logger
import PurusPkg.Package (Package, Version (Version), VersionConstraint)
import PurusPkg.Package qualified as Package

import GHC.Generics (Generic)

{- | A list of 'Registry's that operate as a stack s.t. we look for the package
in the topmost registry going downwards until it doesn't exist
-}
newtype Registries = Registries {getRegistries :: [Registry]}

data RegistryBadLookup = RegistryBadLookup {rblName :: Text, rblVersion :: Version, rblMessage :: String}
  deriving stock (Eq, Ord, Show)

instance Exception RegistryBadLookup where
  displayException = registryBadLookupToString

registryBadLookupToString :: RegistryBadLookup -> String
registryBadLookupToString (RegistryBadLookup {rblName = name, rblVersion = Version version, rblMessage = message}) =
  "bad registry lookup with package "
    ++ Text.unpack name
    ++ SemVer.toString version
    ++ ": "
    ++ message

foldrRegistries :: forall a. (Registry -> a -> a) -> a -> Registries -> a
foldrRegistries f acc registries = foldr f acc $ getRegistries registries

packageFilePath :: FilePath
packageFilePath = "package.json"

{- | Given a 'Registry', 'Text' (name), and 'Version', this queries the corresponding
'Package'.
-}
queryRegistriesPackage :: Registries -> Text -> Version -> IO Package
queryRegistriesPackage registries name version =
  let
    -- template for errors
    registryBadLookup = RegistryBadLookup {rblName = name, rblVersion = version, rblMessage = ""}

    notFound :: IO Package
    notFound = Exception.throwIO $ registryBadLookup {rblMessage = packageFilePath ++ " doesn't exist in the provided registries"}

    go :: Registry -> IO Package -> IO Package
    go registry acc =
      case registry of
        LocalRegistry {lrFilePath = localRegistryFilePath, lrMapping = LocalRegistryMapping localRegistryMapping} -> case Map.lookup (name, version) localRegistryMapping of
          Just reference -> case reference of
            FileReference filepath ->
              Aeson.eitherDecodeFileStrict' (filepath FilePath.</> packageFilePath) >>= \eitherErrorMessageOrPackage ->
                case eitherErrorMessageOrPackage of
                  Left errMsg -> Exception.throwIO registryBadLookup {rblMessage = errMsg}
                  Right package -> return package
          Nothing -> do
            Logger.logWarn $ Exception.displayException $ registryBadLookup {rblMessage = "package doesn't exist in the local registry at " ++ localRegistryFilePath}
            acc
   in
    foldrRegistries go notFound registries

{- | Given a 'Registry', 'Text' (name), 'Version', and 'FilePath'; this will copy the
sources corresponding to the 'Text' (name) and 'Version' into the provided
'FilePath'
-}
queryRegistriesPackageSources :: Registries -> Text -> Version -> FilePath -> IO ()
queryRegistriesPackageSources registries name version destination =
  let
    -- template for errors
    registryBadLookup = RegistryBadLookup {rblName = name, rblVersion = version, rblMessage = ""}

    notFound :: IO ()
    notFound = Exception.throwIO $ registryBadLookup {rblMessage = "package sources exist in the provided registries"}

    go :: Registry -> IO () -> IO ()
    go registry acc =
      case registry of
        LocalRegistry {lrFilePath = localRegistryFilePath, lrMapping = LocalRegistryMapping localRegistryMapping} -> case Map.lookup (name, version) localRegistryMapping of
          Just reference -> case reference of
            FileReference filepath ->
              Process.callProcess "ln" ["-sfn", filepath, destination] `Exception.catch` \err ->
                Exception.throwIO
                  registryBadLookup
                    { rblMessage = Exception.displayException (err :: Exception.IOException)
                    }
          Nothing -> do
            Logger.logWarn $ Exception.displayException $ registryBadLookup {rblMessage = "package sources doesn't exist in the local registry at " ++ localRegistryFilePath}
            acc
   in
    foldrRegistries go notFound $ registries

{- | 'querySatisfyingVersions' queries all the available versions in the registry
satisfying the given version constraints
-}
queryRegistriesSatisfyingVersions :: Registries -> Text -> Set VersionConstraint -> IO (Set (Text, Down Version))
queryRegistriesSatisfyingVersions registries name versionConstraints =
  let
    go :: Registry -> IO (Set (Text, Down Version)) -> IO (Set (Text, Down Version))
    go registry acc = case registry of
      LocalRegistry {lrMapping = LocalRegistryMapping localRegistryMapping} -> do
        let
          -- NOTE(jaredponn) February 3, 2025: pick an invalid
          -- semantic version s.t. <https://semver.org/> this is
          -- strictly smaller than the smallest valid semantic
          -- version
          (_, namesAndPastMapping) =
            Map.split
              (name, Version $ SemVer.version (-1) 0 0 [] [])
              localRegistryMapping -- NOTE(jaredponn) February 3, 2025: more or less the same
              -- thing but we pick something strictly larger than all valid
              -- packages with the name
          (namesMapping, _) =
            Map.split
              (name <> "\000", Version $ SemVer.version 0 0 0 [] [])
              namesAndPastMapping
          -- TODO(jaredponn) February 3, 2025: there's better ways of
          -- splitting the map based on the constraints (but at that
          -- point we're essentially writing our own SQL query optimizer
          -- :D)
          versionsAndNames =
            Set.fromList
              $ Maybe.mapMaybe
                ( \((candidateName, candidateVersion), _reference) ->
                    if Package.versionSatisfiesVersionConstraints
                      candidateVersion
                      $ Set.toList versionConstraints
                      then Just (candidateName, Down candidateVersion)
                      else Nothing
                )
              $ Map.toList namesMapping
        acc' <- acc
        return $ versionsAndNames <> acc'
   in
    foldrRegistries go mempty $ registries

{- | A 'Registry' is the data to provide a mapping from a name and a version to either:

 - the 'Package' -- see 'queryPackage'

 - the package's sources -- 'queryPackageSources'

Moreover, the data should provide enough information to query all versions
(satisfying a given 'VersionConstraint') of a given name; and for every
name and version pair, it should be able to provide the sources of the file.
-}
data Registry = LocalRegistry {lrFilePath :: FilePath, lrMapping :: LocalRegistryMapping}
  -- TODO(jaredponn) February 2, 2025: this will be a feature implemented
  -- later.
  -- > | RemoteRegistry
  deriving (Generic)

instance Aeson.ToJSON Registry
instance Aeson.FromJSON Registry

newtype LocalRegistryMapping = LocalRegistryMapping {getLocalRegistryMapping :: Map (Text, Version) LocalRegistryReference}
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

-- | A reference to how one can get a package
data LocalRegistryReference
  = FileReference FilePath
  deriving
    ( -- | GitReference String
      Generic
    )

instance Aeson.ToJSON LocalRegistryReference
instance Aeson.FromJSON LocalRegistryReference
