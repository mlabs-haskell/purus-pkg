{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Module: PurusPkg.Package

Exposes the package type and related functionality
-}
module PurusPkg.Package (
  Package (..),
  Version (..),
  VersionConstraint (..),
  versionToText,
  versionFromText,
  versionSatisfiesVersionConstraints,
  versionConstraintToText,
  versionConstraintFromText,
)
where

import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Text (Text)

import Data.Coerce qualified as Coerce
import Data.SemVer qualified as SemVer
import Data.SemVer.Constraint qualified as SemVer.Constraint

-- | 'Package' type which defines a package
data Package = Package
  { pName :: Text
  , pVersion :: Version
  , pDependencies :: Map Text VersionConstraint
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

versionConstraintToText :: VersionConstraint -> Text
versionConstraintToText = SemVer.Constraint.toText . Coerce.coerce

versionConstraintFromText :: Text -> Either String VersionConstraint
versionConstraintFromText = Coerce.coerce . SemVer.Constraint.fromText

instance Aeson.ToJSON VersionConstraint where
  toJSON = Aeson.toJSON . versionConstraintToText

instance Aeson.FromJSON VersionConstraint where
  parseJSON =
    Aeson.withText
      "VersionConstraint"
      ( \text -> case versionConstraintFromText text of
          Left err -> fail err
          Right result -> return result
      )
