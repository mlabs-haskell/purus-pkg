{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Module: PurusPkg.Package

Exposes the package type
-}
module PurusPkg.Package where

import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Text (Text)

import Data.Coerce qualified as Coerce
import Data.SemVer qualified as SemVer
import Data.SemVer.Constraint qualified as SemVer.Constraint

-- | 'Name' is the name of a package (just to serve as documentation)
type Name = Text

-- | 'Package' type which defines a package
data Package = Package
  { pName :: Name
  , pVersion :: Version
  , pDependencies :: Map Name VersionConstraint
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $ \v ->
      Package
        <$> v Aeson..: "name"
        <*> v Aeson..: "version"
        <*> v Aeson..: "dependencies"

instance Aeson.ToJSON Package where
  toJSON package =
    Aeson.object
      [ "name" Aeson..= pName package
      , "version" Aeson..= pVersion package
      , "dependencies" Aeson..= pDependencies package
      ]

newtype Version = Version SemVer.Version
  deriving newtype (Eq, Ord, Show)

versionToText :: Version -> Text
versionToText = SemVer.toText . Coerce.coerce

versionFromText :: Text -> Either String Version
versionFromText = Coerce.coerce . SemVer.fromText

instance Aeson.ToJSON Version where
  toJSON = Aeson.toJSON . versionToText

instance Aeson.FromJSON Version where
  parseJSON =
    Aeson.withText
      "Version"
      ( \text -> case fmap Coerce.coerce . SemVer.fromText $ text of
          Left err -> fail err
          Right result -> return result
      )

newtype VersionConstraint = VersionConstraint SemVer.Constraint.Constraint
  deriving stock (Eq, Ord, Show)

-- TODO(jaredponn) February 5, 2025: we need this Ord instance to have sets of
-- constraints, so we just orphan it for now s.t. we can use automatic deriving
-- to write it for us
deriving stock instance Ord SemVer.Constraint.Constraint

{- | returns true iff the provided 'Version' satisfies all the given
'VersionConstraint's
-}
versionSatisfiesVersionConstraints :: Version -> [VersionConstraint] -> Bool
versionSatisfiesVersionConstraints (Version version) versionConstraints =
  all (SemVer.Constraint.satisfies version) $
    map (\(VersionConstraint versionConstraint) -> versionConstraint) versionConstraints

instance Aeson.ToJSON VersionConstraint where
  toJSON versionConstraint = Aeson.toJSON $ SemVer.Constraint.toText $ Coerce.coerce versionConstraint

instance Aeson.FromJSON VersionConstraint where
  parseJSON =
    Aeson.withText
      "VersionConstraint"
      ( \text -> case fmap Coerce.coerce . SemVer.Constraint.fromText $ text of
          Left err -> fail err
          Right result -> return result
      )
