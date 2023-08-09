-- | Analyze module dependencies using `.hi` files compiled with `-fwrite-if-simplified-core`.
module CabalAuditIface where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Map qualified as Map

import CabalAudit.Analysis
import CabalAudit.Command
import CabalAudit.Core (DeclarationFS (..), Dependencies, getDependenciesFromCoreBinds)
import GhcExtras

doAnalyze :: [ModuleName] -> IO Analysis
doAnalyze rootModules = runGhcWithEnv do
    runAnalysis do
        -- Load root modules
        let mkModuleInfo moduleName = ModuleFS moduleName Nothing
        (rootDependencies :: Dependencies) <- concat <$> traverse readModuleDependencies (mkModuleInfo <$> rootModules)
        setRootDeclarations (Map.fromList rootDependencies)
        -- Load dependencies
        traverse_ go (concat $ snd <$> rootDependencies)
  where
    go decl = do
        Map.lookup decl <$> getAnalysis callGraph >>= \case
            Just _ -> pure () -- Already processed
            Nothing -> do
                let modInfo = ModuleFS (mkModuleNameFS decl.declModuleName) (Just decl.declUnitId)
                moduleDecls <- readModuleDependencies modInfo
                declDeps <- case lookup decl moduleDecls of
                    Nothing -> do
                        addUnknownDecl decl
                        pure []
                    Just xs -> pure xs
                addDeclaration decl declDeps
                -- collect the dependencies of the dependencies
                traverse_ go declDeps

readModuleDependencies :: ModuleFS -> StateT Analysis Ghc Dependencies
readModuleDependencies modInfo = lookupOrLoadModule modInfo do
    getCoreBind modInfo.moduleName modInfo.moduleUnit >>= \case
        Nothing -> pure Nothing
        Just (genModule, coreBinds) -> do
            let deps :: Dependencies
                deps = getDependenciesFromCoreBinds genModule coreBinds
            pure (Just deps)

main :: IO ()
main = do
    args <- getArgs
    analysis <- doAnalyze args.rootModules
    checkAnalysis args analysis
