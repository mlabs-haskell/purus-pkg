{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module: PurusPkg.Solver


 Provides functionality and types for solving the version constraints, along
 with creating the final aggregated collection of the solved versions' sources.
-}
module PurusPkg.Solver (solver, MonadSolver (queryPackage, querySatisfyingVersions), SolverIO, runSolverIO, createPurusModules, purusModulesDirectory, NoSatisfyingVersion) where

import PurusPkg.Package (Package (pDependencies), Version, VersionConstraint)
import PurusPkg.Package qualified as Package
import PurusPkg.Registries (Registries)
import PurusPkg.Registries qualified as Registries

import System.Directory qualified as Directory

import Control.Exception (Exception)
import Control.Exception qualified as Exception

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (getDown))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Except qualified as Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader

-- | Error for 'MonadSolver'
data NoSatisfyingVersion = NoSatisfyingVersion {nvName :: Text, nvConstraints :: [VersionConstraint]}
  deriving stock (Eq, Show)

instance Exception NoSatisfyingVersion where
  displayException NoSatisfyingVersion {nvName = name, nvConstraints = constraints} =
    "no satisfying versions for package "
      ++ Text.unpack name
      ++ " given constraints "
      ++ show constraints -- TODO(jaredponn): pretty show the constraints later

{- | 'MonadSolver' isolates the key points of how 'solver' may be
parameterized.

In particular, 'querySatisfyingVersions' returns all the satisfying versions
of a name and set of 'VersionConstraint', and 'queryPackage' must return the
'Package' corresponding to a name and version.
-}
class (MonadError NoSatisfyingVersion m) => MonadSolver m where
  queryPackage :: Text -> Version -> m Package
  querySatisfyingVersions :: Text -> Set VersionConstraint -> m (Set (Text, Down Version))

-- | An instance of 'MonadSolver' using 'PurusPkg.Registries'
newtype SolverIO a = SolverIO (ReaderT Registries (ExceptT NoSatisfyingVersion IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Registries, MonadError NoSatisfyingVersion, MonadIO)

{- |  Unwraps a 'SolverIO' to the underlying 'IO' monad -- throwing an exception
in the case that the solver could not find a satisfying version.
-}
runSolverIO :: SolverIO a -> Registries -> IO a
runSolverIO (SolverIO readerExceptIO) registries = do
  let exceptIO = Reader.runReaderT readerExceptIO registries
  eitherResult <- Except.runExceptT exceptIO
  case eitherResult of
    Left err -> Exception.throwIO err
    Right result -> return result

instance MonadSolver SolverIO where
  queryPackage name version = do
    registries <- Reader.ask
    Reader.liftIO $ Registries.queryRegistriesPackage registries name version

  querySatisfyingVersions name versionConstraints = do
    registries <- Reader.ask
    Reader.liftIO $ Registries.queryRegistriesSatisfyingVersions registries name versionConstraints

solver :: (MonadSolver m) => Package -> m (Map Text Version)
solver package = do
  let
    -- NOTE(jaredponn): this is NP complete, so it's reasonable to believe
    -- the best we can do is brute force with heuristics.
    --
    -- The problem is as follows.
    --
    -- - A *Package* is a
    --      - name
    --      - version (total order -- we use integers in the proof of NP complete)
    --      - mapping from other packages to boolean expressions on
    --        versions (called *constraints*)
    --
    -- So, given a package (call this the *current package*), along with a
    -- list of packages with unique names and versions (called
    -- \*dependencies*), we want to find a subset of the dependencies
    -- (called the *satisfying dependencies*) where each package must have
    -- a unique name (e.g., 2 names with different versions is not allowed)
    -- such that
    --
    --  - every dependency in the current package exists in the satisfying
    --    dependencies, and every satisfying dependency in the satisfying
    --    dependencies satisfy *every* constraint corresponding to its name
    --
    -- This is NP complete since its easy to write a checker for this, and
    -- we can do a Karp reduction to 3SAT.
    --
    -- Given a 3SAT formula of with boolean variables
    --
    --  X1,...,XN
    --
    -- and clauses
    --
    --  C1,...,CM
    --
    -- for each variable Xi, create two packages pXi with versions 0 and 1 with no
    -- dependencies.
    --
    -- Then, for each clause Ci, note that we can enumerate a satisfying
    -- assignment (there are at most 8); so create at most 8 packages pCi
    -- (each being version 1, 2, ..., 8) which depend on the packages
    -- corresponding to the variables of a satisfying assignment choosing
    -- pXi with version 0 (if Xi must be false to make the statement true)
    -- or pXi with version 1 (if Xi must be true to make the statement
    -- true).
    --
    -- Finally, set the current package to the package which depends on all
    -- versions of every package pCi (i=1 .. M).
    --
    -- It's clear that the original boolean formula is satisfied iff the
    -- current package is satisfied which concludes the reduction.
    --
    -- TODO(jaredponn): do some sort of heuristics to make this faster..
    -- its the best we can do
    go :: (MonadSolver m) => Map Text Version -> Map Text (Set VersionConstraint) -> m (Map Text Version)
    go satisfyingVersions dependenciesToSatisfy =
      -- Either we have
      --    1. no dependencies left to satisfy, so we're done
      --    2. there exists some dependencies to satisfy, so we need to
      --       satisfy that dependency..
      case Map.minViewWithKey dependenciesToSatisfy of
        -- 1.
        Nothing -> return satisfyingVersions
        -- 2.
        Just ((name, versionConstraints), remainingDependenciesToSatisfy) ->
          let errorNoSatisfyingVersion = Except.throwError NoSatisfyingVersion {nvName = name, nvConstraints = Set.toList versionConstraints}
           in -- So given a dependency (name and version constraint pair),
              -- we've either:
              --  2.1. already chose the dependency (name) previously, and
              --       just need to check that the previous choice satisfies
              --       the new version constraint introduced
              --  2.2. never have chose the dependency (name) previously, so we
              --       exhaustively check if any of the versions satisfy all
              --       constraints.
              case Map.lookup name satisfyingVersions of
                -- 2.1.
                Just version ->
                  if Package.versionSatisfiesVersionConstraints version $ Set.toList versionConstraints
                    then go satisfyingVersions remainingDependenciesToSatisfy
                    else errorNoSatisfyingVersion
                -- 2.2.
                Nothing ->
                  fmap Set.toList (querySatisfyingVersions name versionConstraints)
                    >>= \versions ->
                      foldr
                        ( \candidateVersion acc -> do
                            candidatePackage <- queryPackage name candidateVersion

                            let candidateDependencies = pDependencies candidatePackage
                            -- since we're adding new dependencies (from
                            -- the candidate package), we need to check
                            -- that the current versions (which satisfy the
                            -- constraints) also satisfy the candidate
                            -- packages constraints
                            if not
                              $ all
                                ( \(chosenPackageName, chosenPackageVersion) ->
                                    maybe True (Package.versionSatisfiesVersionConstraints chosenPackageVersion . List.singleton) $ Map.lookup chosenPackageName candidateDependencies
                                )
                              $ Map.toList satisfyingVersions
                              then errorNoSatisfyingVersion
                              else
                                go
                                  (Map.union satisfyingVersions $ Map.singleton name candidateVersion)
                                  ( Map.unionWith
                                      Set.union
                                      remainingDependenciesToSatisfy
                                      $ Map.map Set.singleton candidateDependencies
                                  )
                                  `Except.catchError` const acc
                        )
                        errorNoSatisfyingVersion
                        $ map (getDown . snd) versions

  satisfyingVersions <- go mempty $ Map.map Set.singleton $ pDependencies package

  return satisfyingVersions

purusModulesDirectory :: FilePath
purusModulesDirectory = "purus-modules"

createPurusModules :: Map Text Version -> Registries -> IO ()
createPurusModules dependencies registries = do
  Directory.createDirectoryIfMissing True purusModulesDirectory
  Directory.withCurrentDirectory purusModulesDirectory $
    Foldable.for_ (Map.toList dependencies) $
      \(name, version) -> do
        let path = Text.unpack $ name <> "-" <> Package.versionToText version
        Registries.queryRegistriesPackageSources registries name version path
