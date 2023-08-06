-- | Analyze module dependencies using `.hi` files compiled with `-fwrite-if-simplified-core`.
module CabalAuditIface where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import GHC hiding (SuccessFlag (..))
import GHC.Core
import GHC.Data.FastString.Env (emptyFsEnv)
import GHC.Data.IOEnv (runIOEnv)
import GHC.Data.Maybe
import GHC.Driver.Env.KnotVars (emptyKnotVars)
import GHC.Driver.Session
import GHC.IfaceToCore
import GHC.Paths (libdir)
import GHC.Tc.Types
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Types.TypeEnv (emptyTypeEnv)

import Data.Foldable
import GHC.Utils.Outputable hiding ((<>))

import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Map.Strict (Map)

import CabalAudit.Command
import CabalAudit.Plugin (DeclarationFS (..), Dependencies, getDependenciesFromCoreBinds)
import Control.Monad.Trans.Class (lift)

data LoaderState = LoaderState
    { modules :: Map ModuleName Dependencies
    , deps :: Map DeclarationFS [DeclarationFS]
    }

doAnalyze :: [ModuleName] -> IO Analysis
doAnalyze rootModules = do
    let db = PkgDbPath "../dist-newstyle/packagedb/ghc-9.6.1"
    let addPackageDB dflags = dflags{packageDBFlags = [PackageDB db]}
    let emptyState = LoaderState mempty mempty
    runGhc (Just libdir) $ flip evalStateT emptyState do
        lift $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags (addPackageDB dflags)
        (rootDependencies :: Dependencies) <- concat <$> traverse readModuleDependencies rootModules
        modify (\s -> s{deps = Map.fromList rootDependencies})
        traverse_ go (concat $ snd <$> rootDependencies)
        callGraph <- Map.toList . deps <$> get
        pure $ Analysis callGraph mempty mempty
  where
    go decl = do
        deps <- getDependencies decl
        traverse_ go deps

getDependencies :: DeclarationFS -> StateT LoaderState Ghc [DeclarationFS]
getDependencies decl = do
    knownDecl <- (Map.lookup decl . deps) <$> get
    case knownDecl of
        Just deps -> pure deps
        Nothing -> do
            moduleDecls <- readModuleDependencies (mkModuleNameFS decl.declModuleName)
            let deps = fromMaybe [] $ lookup decl moduleDecls
            modify (\s -> s{deps = Map.insert decl deps s.deps})
            pure deps

readModuleDependencies :: ModuleName -> StateT LoaderState Ghc Dependencies
readModuleDependencies moduleName = StateT \s -> case Map.lookup moduleName s.modules of
    Just deps -> pure (deps, s)
    Nothing -> do
        (genModule, coreBinds) <- getCoreBind moduleName
        let deps :: Dependencies
            deps = removeUnique $ getDependenciesFromCoreBinds genModule coreBinds
        pure (deps, s{modules = Map.insert moduleName deps s.modules})

getCoreBindFromIFace :: HscEnv -> ModIface -> IO [CoreBind]
getCoreBindFromIFace hscEnv modIface = do
    let ifLclEnv :: IfLclEnv
        ifLclEnv =
            IfLclEnv
                { if_mod = modIface.mi_module
                , if_boot = hscSourceToIsBoot modIface.mi_hsc_src
                , if_loc = ""
                , if_nsubst = Nothing
                , if_implicits_env = Nothing
                , if_tv_env = emptyFsEnv
                , if_id_env = emptyFsEnv
                }
        ifGblEnv :: IfGblEnv
        ifGblEnv = IfGblEnv "" emptyKnotVars
    case modIface.mi_extra_decls of
        Nothing -> do
            liftIO $ putStrLn $ "Module doesn't have simplified core! (missing -fwrite-if-simplified-core?)"
            pure []
        Just decls -> do
            typeEnv <- newIORef emptyTypeEnv
            runIOEnv (Env hscEnv 'a' ifGblEnv ifLclEnv) do
                coreBinds <- tcTopIfaceBindings typeEnv decls
                liftIO $ putStrLn $ "Converted " <> show (length decls) <> " decl into " <> show (length coreBinds)
                pure coreBinds

getCoreBind :: ModuleName -> Ghc (Module, [CoreBind])
getCoreBind moduleName = do
    liftIO $ putStrLn $ "Loading module " <> show moduleName
    genModule <- lookupModule moduleName Nothing
    getModuleInfo genModule >>= \case
        Nothing -> do
            liftIO $ putStrLn $ "Module doesn't have info!"
            pure (genModule, [])
        Just modInfo -> case modInfoIface modInfo of
            Nothing -> do
                liftIO $ putStrLn $ "Module info doesn't have iface!"
                pure (genModule, [])
            Just modIface -> do
                hscEnv <- getSession
                liftIO do
                    coreBinds <- getCoreBindFromIFace hscEnv modIface
                    pure (genModule, coreBinds)

main :: IO ()
main = do
    args <- getArgs
    analysis <- doAnalyze args.rootModules
    checkAnalysis args analysis

showPpr :: SDoc -> String
showPpr = showSDocOneLine defaultSDocContext
