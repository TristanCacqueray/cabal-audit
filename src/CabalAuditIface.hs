-- This does not work, 'tcTopIfaceBindings' fails because it can't find the unit
module CabalAuditIface where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import GHC hiding (SuccessFlag (..))
import GHC.Core
import GHC.Data.FastString.Env (emptyFsEnv)
import GHC.Data.IOEnv (runIOEnv)
import GHC.Data.Maybe
import GHC.Driver.Env.KnotVars (emptyKnotVars)
import GHC.Driver.Env.Types
import GHC.Driver.Session
import GHC.Iface.Load (readIface)
import GHC.IfaceToCore
import GHC.Tc.Types
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Types.TypeEnv (emptyTypeEnv)

import CabalAudit (showPpr)
import Data.Foldable
import GHC.Unit.Types (UnitId (UnitId), stringToUnit)
import GHC.Utils.Outputable (ppr)

getCoreBind :: ModIface -> Ghc [CoreBind]
getCoreBind modIface = do
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
        Nothing -> pure []
        Just decls -> do
            hscEnv <- getSession
            typeEnv <- liftIO (newIORef emptyTypeEnv)
            liftIO do
                putStrLn $ "Converting " <> show (length decls)
                runIOEnv (Env hscEnv 'a' ifGblEnv ifLclEnv) do
                    tcTopIfaceBindings typeEnv decls

loadIface :: ModuleName -> FilePath -> Ghc ModIface
loadIface moduleName fp = do
    let genModule = mkModule (stringToUnit "main") moduleName
    dynFlags <- getDynFlags
    hscEnv <- getSession
    liftIO (readIface dynFlags hscEnv.hsc_NC genModule fp) >>= \case
        Succeeded v -> pure v
        Failed err -> error ("readIface failed: " <> showPpr (ppr err))

main :: IO ()
main = do
    runGhc Nothing do
        liftIO $ putStrLn $ "Loading iface..."
        let fp = "dist-newstyle/build/x86_64-linux/ghc-9.6.1/cabal-audit-0.1/t/spec/noopt/build/spec/spec-tmp/CabalAudit/Test/Simple.hi"
            modName = "CabalAudit.Test.Simple"
        target <- guessTarget modName (Just $ UnitId "main") Nothing
        liftIO $ putStrLn $ "Target: " <> showPpr (ppr target)
        addTarget target
        modIface <- loadIface (mkModuleName "CabalAudit.Test.Simple") fp
        liftIO $ putStrLn $ "Got iface: " <> showPpr (ppr modIface.mi_extra_decls)
        coreBinds <- getCoreBind modIface
        liftIO do
            putStrLn $ "Corebinds " <> show (length coreBinds)
            traverse_ (putStrLn . showPpr . ppr) coreBinds
