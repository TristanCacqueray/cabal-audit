-- | Analyze module dependencies using `.hi` files compiled with `-fwrite-if-simplified-core`.
module CabalAuditIface where

import Control.Exception (SomeException)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IORef (newIORef)
import Data.Map qualified as Map
import GHC hiding (SuccessFlag (..))
import GHC.Core
import GHC.Data.FastString.Env (emptyFsEnv)
import GHC.Data.IOEnv (runIOEnv)
import GHC.Driver.Env.KnotVars (emptyKnotVars)
import GHC.Driver.Session
import GHC.IfaceToCore
import GHC.Paths (libdir)
import GHC.Tc.Types
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Types.TypeEnv (emptyTypeEnv)

import CabalAudit.Analysis
import CabalAudit.Command
import CabalAudit.Plugin (DeclarationFS (..), Dependencies, getDependenciesFromCoreBinds)

doAnalyze :: [ModuleName] -> IO Analysis
doAnalyze rootModules = do
    let db = PkgDbPath "../dist-newstyle/packagedb/ghc-9.6.1"
    let addPackageDB dflags = dflags{packageDBFlags = [PackageDB db]}
    runGhc (Just libdir) $ runAnalysis do
        lift $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags (addPackageDB dflags)
        let mkModuleInfo moduleName = ModuleFS moduleName Nothing
        (rootDependencies :: Dependencies) <- concat <$> traverse readModuleDependencies (mkModuleInfo <$> rootModules)
        setRootDeclarations (Map.fromList rootDependencies)
        traverse_ go (concat $ snd <$> rootDependencies)
  where
    go decl = do
        deps <- getDependencies decl
        traverse_ go deps

getDependencies :: DeclarationFS -> StateT Analysis Ghc [DeclarationFS]
getDependencies decl =
    Map.lookup decl <$> getAnalysis callGraph >>= \case
        Just deps -> pure deps
        Nothing -> do
            let modInfo = ModuleFS (mkModuleNameFS decl.declModuleName) (Just decl.declUnitId)
            moduleDecls <- readModuleDependencies modInfo
            deps <- case lookup decl moduleDecls of
                Nothing -> do
                    addUnknownDecl decl
                    pure []
                Just xs -> pure xs
            addDeclaration decl deps
            pure deps

readModuleDependencies :: ModuleFS -> StateT Analysis Ghc Dependencies
readModuleDependencies modInfo = lookupOrLoadModule modInfo do
    getCoreBind modInfo >>= \case
        Nothing -> pure Nothing
        Just (genModule, coreBinds) -> do
            let deps :: Dependencies
                deps = removeUnique $ getDependenciesFromCoreBinds genModule coreBinds
            pure (Just deps)

getCoreBindFromIFace :: HscEnv -> ModIface -> IO (Maybe [CoreBind])
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
            pure Nothing
        Just decls -> do
            typeEnv <- newIORef emptyTypeEnv
            runIOEnv (Env hscEnv 'a' ifGblEnv ifLclEnv) do
                coreBinds <- tcTopIfaceBindings typeEnv decls
                liftIO $ putStrLn $ "Converted " <> show (length decls) <> " decl into " <> show (length coreBinds)
                pure (Just coreBinds)

getCoreBind :: ModuleFS -> Ghc (Maybe (Module, [CoreBind]))
getCoreBind modInfo = do
    try (unsafeGetCore modInfo) >>= \case
        Left (e :: SomeException) -> do
            liftIO $ putStrLn $ "Loading error: " <> show e
            pure Nothing
        Right n -> pure n

-- | Throw an error when the module if unknown
unsafeGetCore :: ModuleFS -> Ghc (Maybe (Module, [CoreBind]))
unsafeGetCore modInfo = do
    liftIO $ putStrLn $ "Loading module " <> show modInfo
    genModule <- GHC.lookupModule modInfo.moduleName modInfo.moduleUnit
    getModuleInfo genModule >>= \case
        Nothing -> do
            liftIO $ putStrLn $ "Module doesn't have info!"
            pure Nothing
        Just mInfo -> case modInfoIface mInfo of
            Nothing -> do
                liftIO $ putStrLn $ "Module info doesn't have iface!"
                pure Nothing
            Just modIface -> do
                hscEnv <- getSession
                liftIO $
                    getCoreBindFromIFace hscEnv modIface >>= \case
                        Nothing -> pure Nothing
                        Just coreBinds -> pure (Just (genModule, coreBinds))

main :: IO ()
main = do
    args <- getArgs
    analysis <- doAnalyze args.rootModules
    checkAnalysis args analysis
