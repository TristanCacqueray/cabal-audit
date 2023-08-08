module CabalAudit.Analysis where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), execStateT, get)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Data.FastString (FastString, NonDetFastString (..), unpackFS)
import GHC.Unit.Module (ModuleName, moduleNameString)

import CabalAudit.Core

data ModuleFS = ModuleFS
    { moduleName :: ModuleName
    , moduleUnit :: Maybe FastString
    }
    deriving (Eq)

instance Ord ModuleFS where
    compare m1 m2 =
        compare m1.moduleName m2.moduleName
            <> compare (NonDetFastString <$> m1.moduleUnit) (NonDetFastString <$> m2.moduleUnit)

instance Show ModuleFS where
    show mi = miUnit <> moduleNameString mi.moduleName
      where
        miUnit = case mi.moduleUnit of
            Nothing -> ""
            Just fs -> unpackFS fs <> ":"

data Analysis = Analysis
    { callGraph :: Map DeclarationFS [DeclarationFS]
    , missingModules :: Set ModuleFS
    , unknownDecls :: Set DeclarationFS
    , knownModules :: Map ModuleFS Dependencies
    }

getAnalysis :: (Monad m) => (Analysis -> a) -> StateT Analysis m a
getAnalysis selector = selector <$> get

emptyAnalysis :: Analysis
emptyAnalysis = Analysis mempty mempty mempty mempty

runAnalysis :: (Monad m) => StateT Analysis m () -> m Analysis
runAnalysis = flip execStateT emptyAnalysis

lookupOrLoadModule :: (Monad m) => ModuleFS -> m (Maybe Dependencies) -> StateT Analysis m Dependencies
lookupOrLoadModule moduleInfo loadModule = do
    Map.lookup moduleInfo <$> getAnalysis knownModules >>= \case
        Just deps -> pure deps
        Nothing -> do
            deps <-
                lift loadModule >>= \case
                    Just deps -> pure deps
                    Nothing -> do
                        addMissingModule moduleInfo
                        pure []
            addKnownModules moduleInfo deps
            pure deps

addKnownModules :: (Monad m) => ModuleFS -> Dependencies -> StateT Analysis m ()
addKnownModules key value = StateT \s -> pure ((), s{knownModules = Map.insert key value s.knownModules})

addUnknownDecl :: (Monad m) => DeclarationFS -> StateT Analysis m ()
addUnknownDecl decl = StateT \s -> pure ((), s{unknownDecls = Set.insert decl s.unknownDecls})

addMissingModule :: (Monad m) => ModuleFS -> StateT Analysis m ()
addMissingModule mi = StateT \s -> pure ((), s{missingModules = Set.insert mi s.missingModules})

addDeclaration :: (Monad m) => DeclarationFS -> [DeclarationFS] -> StateT Analysis m ()
addDeclaration key value = StateT \s -> pure ((), s{callGraph = Map.insert key value s.callGraph})

setRootDeclarations :: (Monad m) => Map DeclarationFS [DeclarationFS] -> StateT Analysis m ()
setRootDeclarations decls = StateT \s -> pure ((), s{callGraph = decls})
