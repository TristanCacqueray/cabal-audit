{-# LANGUAGE GADTs #-}

module CabalAudit.Plugin (
    plugin,
) where

import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Pipeline (TPhase (..))
import GHC.Driver.Pipeline.Execute (runPhase)
import GHC.Driver.Pipeline.Phases (PhaseHook (..))
import GHC.IO (unsafePerformIO)
import GHC.Plugins hiding ((<>))

import CabalAudit.Core
import Data.Binary qualified as Binary

plugin :: Plugin
plugin =
    defaultPlugin
        { -- Install pass to record core call graph in 'globalEnvIORef'
          installCoreToDos = \_ todo -> pure (CoreDoPluginPass "Collect Dependencies" pass : todo)
        , -- Install hook to dump the call graph next to the '.hi' file
          driverPlugin = \_ hscEnv ->
            let hooks = hscEnv.hsc_hooks{runPhaseHook = Just (PhaseHook runPhaseFun)}
             in pure $ hscEnv{hsc_hooks = hooks}
        }

{-# NOINLINE globalEnvIORef #-}
globalEnvIORef :: IORef (Map.Map ModuleName Dependencies)
globalEnvIORef = unsafePerformIO $ newIORef mempty

-- | Pass collects the dependencies and register them to the global env for the driverFun
pass :: ModGuts -> CoreM ModGuts
pass guts = do
    let dependencies :: Dependencies
        dependencies = getDependenciesFromCoreBinds guts.mg_module guts.mg_binds

    liftIO do
        putStrLn $ " XX  Processed core for " <> show (moduleName guts.mg_module) <> ": " <> show (length dependencies)
        -- printDependencies dependencies
        modifyIORef globalEnvIORef $ Map.insert (moduleName guts.mg_module) dependencies

    pure guts

-- | Serialize the collected dependencies (from the global env) next to the .hi file
runPhaseFun :: TPhase a -> IO a
runPhaseFun phase = do
    let _phaseStr :: String
        _phaseStr = case phase of
            T_Unlit{} -> "T_Unlit"
            T_FileArgs _hscEnv fp -> "T_FileArgs: " <> fp
            T_Cpp{} -> "T_Cpp"
            T_HsPp{} -> "T_HsPp"
            T_HscRecomp{} -> "T_HscRecomp"
            T_Hsc{} -> "T_Hsc"
            T_HscPostTc{} -> "T_HscPostTc"
            T_HscBackend _pipeEnv _hscEnv _modName _hscSource _modLocation _action -> "T_HscBackend: " <> show _modName
            T_CmmCpp{} -> "T_CmmCpp"
            T_Cmm{} -> "T_Cmm"
            T_Cc{} -> "T_Cc"
            T_As{} -> "T_As"
            T_LlvmOpt{} -> "T_LlvmOpt"
            T_LlvmLlc{} -> "T_LlvmLlc"
            T_LlvmMangle{} -> "T_LlvmMangle"
            T_MergeForeign{} -> "T_MergeForeign"
            _ -> "?"

    -- putStrLn $ " XXX:  runPhaseFun phase: " <> phaseStr
    case phase of
        T_HscBackend _pipeEnv _hscEnv modName _hscSource modLocation _action -> do
            result <- runPhase phase
            dependencies <- fromMaybe [] . Map.lookup modName <$> readIORef globalEnvIORef
            liftIO $ writeDependencies (modLocation.ml_hi_file <> "x") dependencies
            pure result
        _ -> runPhase phase

writeDependencies :: FilePath -> Dependencies -> IO ()
writeDependencies fp dependencies = do
    putStrLn $ " XXX Writing " <> show (length dependencies) <> " dependencies info to: " <> fp
    Binary.encodeFile fp dependencies
