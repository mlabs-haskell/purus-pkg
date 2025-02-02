{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

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

instance Aeson.ToJSON Version where
  toJSON version = Aeson.toJSON $ SemVer.toText $ Coerce.coerce version

instance Aeson.FromJSON Version where
  parseJSON =
    Aeson.withText
      "Version"
      ( \text -> case fmap Coerce.coerce . SemVer.fromText $ text of
          Left err -> fail err
          Right result -> return result
      )

newtype VersionConstraint = VersionConstraint SemVer.Constraint.Constraint
  deriving stock (Eq, Show)

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
