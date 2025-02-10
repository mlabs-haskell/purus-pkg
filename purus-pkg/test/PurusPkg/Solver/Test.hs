{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module PurusPkg.Solver.Test where

import Control.Exception qualified as Exception
import Data.Coerce qualified as Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Test.Tasty (TestTree)
import Test.Tasty qualified
import Test.Tasty.HUnit qualified

import PurusPkg.Package (Package (Package, pDependencies, pName, pVersion), Version (Version), VersionConstraint (VersionConstraint))
import PurusPkg.Package qualified
import PurusPkg.Solver (MonadSolver, NoSatisfyingVersion)
import PurusPkg.Solver qualified

import Control.Monad.Except (Except, MonadError)
import Control.Monad.Except qualified as Except
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader

import Data.SemVer qualified as SemVer
import Data.SemVer.Constraint qualified as SemVer.Constraint
import Data.Text (Text)

-- | A mock data type for the registry for 'MockSolver'
newtype MockRegistry = MockRegistry {getMockRegistry :: Map (Text, Version) Package}
  deriving newtype (Show, Eq)

-- | Helper for making key value pairs of the 'MockRegistry'
makeMockRegistryPackage :: Text -> Version -> Map Text VersionConstraint -> ((Text, Version), Package)
makeMockRegistryPackage name version dependencies =
  ( (name, version)
  , Package {pName = name, pVersion = version, pDependencies = dependencies}
  )

-- | An instance of 'MonadSolver' for mocking up tests
newtype MockSolver a = MockSolver (ReaderT MockRegistry (Except NoSatisfyingVersion) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader MockRegistry, MonadError NoSatisfyingVersion)

runMockSolver :: MockSolver a -> MockRegistry -> Either NoSatisfyingVersion a
runMockSolver (MockSolver mockSolver) mockRegistry = Except.runExcept (Reader.runReaderT mockSolver mockRegistry)

instance MonadSolver MockSolver where
  queryPackage name version = do
    mockRegistry <- Reader.ask
    case Map.lookup (name, version) $ getMockRegistry mockRegistry of
      Just result -> return result
      Nothing ->
        error $
          "package " ++ show (name, version) ++ " not found in mock registry: " ++ show mockRegistry

  querySatisfyingVersions name versionConstraints = do
    mockRegistry <- Reader.ask
    return
      $ Set.fromList
      $ Maybe.mapMaybe
        ( \((candidateName, candidateVersion), _) ->
            if candidateName == name
              && PurusPkg.Package.versionSatisfiesVersionConstraints
                candidateVersion
                (Set.toList versionConstraints)
              then Just (candidateName, Down candidateVersion)
              else Nothing
        )
      $ Map.toList
      $ getMockRegistry mockRegistry

{- | 'checkDependencies' checks that the provided dependencies (mapping of
names to versions) satisfies all version constraints of the package and all
dependencies
-}
checkDependencies :: Map Text Version -> Package -> MockRegistry -> Bool
checkDependencies dependencies package mockRegistry =
  let
    arePackageDependenciesSatisfied :: Package -> Bool
    arePackageDependenciesSatisfied pkg =
      let pkgDeps = PurusPkg.Package.pDependencies pkg
       in all
            ( \(pkgDepName, pkgDepVersionConstraint) ->
                case Map.lookup pkgDepName dependencies of
                  Nothing -> False
                  Just dependencyVersion -> PurusPkg.Package.versionSatisfiesVersionConstraints dependencyVersion [pkgDepVersionConstraint]
            )
            $ Map.toList pkgDeps
   in
    -- are all the dependencies of the provided package satisfied?
    arePackageDependenciesSatisfied package
      &&
      -- are all the dependencies of the dependencies of the packages also satisfied?
      all
        ( \(depName, depVersion) ->
            case Map.lookup (depName, depVersion) $ getMockRegistry mockRegistry of
              Nothing -> False
              Just pkg -> arePackageDependenciesSatisfied pkg
        )
        (Map.toList dependencies)

tests :: TestTree
tests =
  Test.Tasty.testGroup
    "Solver tests"
    [ solvableUnitTests
    , unsolvableUnitTests
    ]

solvableUnitTests :: TestTree
solvableUnitTests =
  Test.Tasty.testGroup
    "Solvable unit tests"
    [ let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    , ("B", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    , ("C", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 1 0 0 [] [])) mempty
                , makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "A" (Version (SemVer.version 3 0 0 [] [])) mempty
                , makeMockRegistryPackage "B" (Version (SemVer.version 1 0 0 [] [])) mempty
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "B" (Version (SemVer.version 3 0 0 [] [])) mempty
                , makeMockRegistryPackage "C" (Version (SemVer.version 1 0 0 [] [])) mempty
                , makeMockRegistryPackage "C" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "C" (Version (SemVer.version 3 0 0 [] [])) mempty
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should be satisfied 1" $ do
            chosenDependencies <- either Exception.throwIO return eitherResult
            Test.Tasty.HUnit.assertBool "Packages are not satisfied" $ checkDependencies chosenDependencies package mockRegistry
    , let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CAny)
                    , ("B", VersionConstraint $ SemVer.Constraint.CAny)
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "D" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      []
                , makeMockRegistryPackage "D" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      []
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should be satisfied 2" $ do
            chosenDependencies <- either Exception.throwIO return eitherResult
            Test.Tasty.HUnit.assertEqual
              "checking that chosen package A is version 2.0.0"
              (Just $ SemVer.version 1 0 0 [] [])
              $ Coerce.coerce
              $ Map.lookup "A" chosenDependencies
            Test.Tasty.HUnit.assertBool "Packages are not satisfied" $ checkDependencies chosenDependencies package mockRegistry
    , let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CAny)
                    , ("B", VersionConstraint $ SemVer.Constraint.CAny)
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      , ("B", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "D" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      []
                , makeMockRegistryPackage "D" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      []
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should be satisfied 3" $ do
            chosenDependencies <- either Exception.throwIO return eitherResult
            Test.Tasty.HUnit.assertEqual
              "checking that chosen package A is version 2.0.0"
              (Just $ SemVer.version 2 0 0 [] [])
              $ Coerce.coerce
              $ Map.lookup "A" chosenDependencies
            Test.Tasty.HUnit.assertEqual
              "checking that chosen package B is version 2.0.0"
              (Just $ SemVer.version 2 0 0 [] [])
              $ Coerce.coerce
              $ Map.lookup "B" chosenDependencies
            Test.Tasty.HUnit.assertEqual
              "checking that chosen package D is version 2.0.0"
              (Just $ SemVer.version 2 0 0 [] [])
              $ Coerce.coerce
              $ Map.lookup "D" chosenDependencies
            Test.Tasty.HUnit.assertBool "Packages are not satisfied" $ checkDependencies chosenDependencies package mockRegistry
    ]

unsolvableUnitTests :: TestTree
unsolvableUnitTests =
  Test.Tasty.testGroup
    "Unsolvable unit tests (expect failure)"
    [ let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    , ("B", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    , ("C", VersionConstraint $ SemVer.Constraint.CEq (SemVer.version 1 0 0 [] []))
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "A" (Version (SemVer.version 3 0 0 [] [])) mempty
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "B" (Version (SemVer.version 3 0 0 [] [])) mempty
                , makeMockRegistryPackage "C" (Version (SemVer.version 2 0 0 [] [])) mempty
                , makeMockRegistryPackage "C" (Version (SemVer.version 3 0 0 [] [])) mempty
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should NOT be satisfied 1" $ do
            Test.Tasty.HUnit.assertBool ("Packages are satisfied with " ++ show eitherResult) $
              case eitherResult of
                Left {} -> True
                Right {} -> False
    , let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CAny)
                    , ("B", VersionConstraint $ SemVer.Constraint.CAny)
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "D" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      []
                , makeMockRegistryPackage "D" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      []
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should NOT be satisfied 2" $ do
            Test.Tasty.HUnit.assertBool ("Packages are satisfied with " ++ show eitherResult) $
              case eitherResult of
                Left {} -> True
                Right {} -> False
    , let package =
            Package
              { pName = "TOP"
              , pVersion = Version $ SemVer.version 1 0 0 [] []
              , pDependencies =
                  Map.fromList
                    [ ("A", VersionConstraint $ SemVer.Constraint.CAny)
                    , ("B", VersionConstraint $ SemVer.Constraint.CAny)
                    ]
              }
          mockRegistry =
            MockRegistry $
              Map.fromList
                [ makeMockRegistryPackage "A" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      , ("B", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "A" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 2 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "B" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      [ ("D", VersionConstraint $ SemVer.Constraint.CEq $ SemVer.version 1 0 0 [] [])
                      ]
                , makeMockRegistryPackage "D" (Version (SemVer.version 1 0 0 [] [])) $
                    Map.fromList
                      []
                , makeMockRegistryPackage "D" (Version (SemVer.version 2 0 0 [] [])) $
                    Map.fromList
                      []
                ]
          eitherResult = runMockSolver (PurusPkg.Solver.solver package) mockRegistry
       in Test.Tasty.HUnit.testCase "Package and MockRegistry should NOT be satisfied 3" $ do
            Test.Tasty.HUnit.assertBool ("Packages are satisfied with " ++ show eitherResult) $
              case eitherResult of
                Left {} -> True
                Right {} -> False
    ]
