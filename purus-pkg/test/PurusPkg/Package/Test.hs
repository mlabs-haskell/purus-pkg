module PurusPkg.Package.Test (tests) where

import PurusPkg.Package (Package (Package, pDependencies, pName, pVersion), Version (Version), VersionConstraint (VersionConstraint))

import Data.SemVer qualified
import Data.SemVer.Constraint qualified

import Test.Tasty (TestTree)
import Test.Tasty qualified
import Test.Tasty.QuickCheck (Gen)
import Test.Tasty.QuickCheck qualified

import Data.Aeson qualified as Aeson
import Data.Coerce qualified as Coerce
import Data.Map qualified as Map
import Data.Text qualified as Text

tests :: TestTree
tests =
  Test.Tasty.testGroup
    "Package tests"
    [ Test.Tasty.QuickCheck.testProperty "Roundtrip tests: Package --> JSON --> Package" $
        Test.Tasty.QuickCheck.forAll genPackage $
          \pkg -> Right pkg Test.Tasty.QuickCheck.=== Aeson.eitherDecode' (Aeson.encode pkg)
    ]

genPackage :: Gen Package
genPackage = do
  name <- fmap Text.pack $ Test.Tasty.QuickCheck.arbitrary
  version <- genVersion

  dependencies <- fmap (Map.mapKeys Text.pack) $ Test.Tasty.QuickCheck.liftArbitrary genVersionConstraint

  return Package {pName = name, pVersion = version, pDependencies = dependencies}

genVersion :: Gen Version
genVersion = do
  Test.Tasty.QuickCheck.Positive major <- Test.Tasty.QuickCheck.arbitrary
  Test.Tasty.QuickCheck.Positive minor <- Test.Tasty.QuickCheck.arbitrary
  Test.Tasty.QuickCheck.Positive patch <- Test.Tasty.QuickCheck.arbitrary
  return $
    -- TODO(jaredponn) February 1, 2025: it would be good to include random
    -- generation of the pre-release and build parts
    Version $
      Data.SemVer.version major minor patch [] []

genVersionConstraint :: Gen VersionConstraint
genVersionConstraint =
  fmap VersionConstraint $
    Test.Tasty.QuickCheck.oneof
      [ return Data.SemVer.Constraint.CAny
      , Data.SemVer.Constraint.CLt <$> fmap Coerce.coerce genVersion
      , Data.SemVer.Constraint.CLtEq <$> fmap Coerce.coerce genVersion
      , Data.SemVer.Constraint.CGtEq <$> fmap Coerce.coerce genVersion
      , Data.SemVer.Constraint.CEq <$> fmap Coerce.coerce genVersion
      -- TODO(jaredponn) February 1, 2025: it would be good to include random
      -- generation of the last few constructors
      -- 'Data.SemVer.Constraint.CAnd' and 'Data.SemVer.Constraint.COr'
      ]
