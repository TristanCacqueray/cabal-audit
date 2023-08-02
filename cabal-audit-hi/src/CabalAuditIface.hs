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
import GHC.Driver.Env.Types
import GHC.Driver.Session
import GHC.Iface.Load (readIface)
import GHC.IfaceToCore
import GHC.Paths (libdir)
import GHC.Tc.Types
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Types.TypeEnv (emptyTypeEnv)

import Data.Foldable
import GHC.Unit.Types (stringToUnit)
import GHC.Utils.Outputable hiding ((<>))

getCoreBind :: HscEnv -> ModIface -> IO [CoreBind]
getCoreBind hscEnv modIface = do
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
            typeEnv <- newIORef emptyTypeEnv
            putStrLn $ "Converting " <> show (length decls)
            runIOEnv (Env hscEnv 'a' ifGblEnv ifLclEnv) do
                tcTopIfaceBindings typeEnv decls

-- This does not seems to work well, bad unit id?
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
    runGhc (Just libdir) do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        liftIO $ putStrLn $ "Lookup module..."
        genModule <- lookupModule (mkModuleName "Data.Void") Nothing
        liftIO $ putStrLn $ "get module info"
        Just modInfo <- getModuleInfo genModule

        case modInfoIface modInfo of
            Nothing -> liftIO $ putStrLn "No iface?"
            Just modIface -> do
                hscEnv <- getSession
                liftIO do
                    putStrLn $ "Got iface: " <> showPpr (ppr modIface.mi_extra_decls)
                    coreBinds <- getCoreBind hscEnv modIface
                    putStrLn $ "Corebinds " <> show (length coreBinds)
                    traverse_ (putStrLn . showPpr . ppr) coreBinds

showPpr :: SDoc -> String
showPpr = showSDocOneLine defaultSDocContext
