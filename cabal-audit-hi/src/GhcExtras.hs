module GhcExtras (
    runGhcWithEnv,
    getCoreBind,

    -- * Re-export
    Ghc,
    ModuleName,
    mkModuleNameFS,
) where

import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import GHC hiding (SuccessFlag (..))
import GHC.Core
import GHC.Data.Bag qualified as Bag
import GHC.Data.FastString.Env (emptyFsEnv)
import GHC.Data.IOEnv (runIOEnv)
import GHC.Driver.CmdLine (Err (errMsg), Warn (warnMsg), runEwM)
import GHC.Driver.Env.KnotVars (emptyKnotVars)
import GHC.Driver.Session
import GHC.IfaceToCore
import GHC.Paths (libdir)
import GHC.Platform (Platform (platformArchOS))
import GHC.Tc.Types
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Types.TypeEnv (emptyTypeEnv)

import Data.Maybe (fromMaybe)
import GHC.Data.FastString (FastString)

-- | Setup a Ghc session using the packages found in the local environment file
runGhcWithEnv :: Ghc a -> IO a
runGhcWithEnv action =
    runGhc (Just libdir) do
        -- Prepare the ghc session using the local environment
        dflags <- getSessionDynFlags
        let envPath = ".ghc.environment." <> versionedFilePath dflags.targetPlatform.platformArchOS
        envFile <- liftIO $ readFile envPath
        setSessionDynFlags (doSetEnvFileFlags dflags envPath envFile)
        action
  where
    doSetEnvFileFlags :: DynFlags -> FilePath -> String -> DynFlags
    doSetEnvFileFlags dflags envPath envFile = snd (runCmdLineP readEnvFile dflags)
      where
        readEnvFile :: CmdLineP DynFlags ()
        readEnvFile = do
            (errs, warns, ()) <- runEwM (setFlagsFromEnvFile envPath envFile)
            unless (Bag.isEmptyBag warns && Bag.isEmptyBag errs) do
                let errMsgs = errMsg <$> Bag.bagToList errs
                    warnMsgs = warnMsg <$> Bag.bagToList warns
                error $ "Warnings: " <> show warnMsgs <> "\nErrors: " <> show errMsgs
            pure ()

-- | Lookup a module and extract the simplified core.
getCoreBind :: ModuleName -> Maybe FastString -> Ghc (Maybe (Module, [CoreBind]))
getCoreBind moduleName mPkgId = do
    try (unsafeGetCore moduleName mPkgId) >>= \case
        Left (e :: SomeException) -> do
            liftIO $ putStrLn $ "Loading error: " <> show e
            pure Nothing
        Right n -> pure n

-- | The 'getCoreBind' implementation, but throwing an exception from 'GHC.lookupModule'.
unsafeGetCore :: ModuleName -> Maybe FastString -> Ghc (Maybe (Module, [CoreBind]))
unsafeGetCore moduleName mPkgId = do
    liftIO $ putStrLn $ "Loading module " <> show (fromMaybe "" mPkgId) <> ":" <> show mPkgId
    genModule <- GHC.lookupModule moduleName mPkgId
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

-- | Convert the simplified core from ModIface into regular core.
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
