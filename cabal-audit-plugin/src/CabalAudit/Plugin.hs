{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CabalAudit.Plugin (
    plugin,
    getDependenciesFromCoreBinds,
    DeclarationFS (..),
    Dependencies,
    printDependencies,
    readDependencies,
) where

import Control.Monad
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Pipeline (TPhase (..))
import GHC.Driver.Pipeline.Execute (runPhase)
import GHC.Driver.Pipeline.Phases (PhaseHook (..))
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Plugins hiding ((<>))

import Data.Binary qualified as Binary
import GHC.Types.Unique (getKey)

data DeclarationFS = DeclarationFS
    { declModuleName :: FastString
    , declUnitId :: FastString
    , declOccName :: FastString
    , declUnique :: Int
    }
    deriving (Eq, Generic, Ord)

type Dependencies = [(DeclarationFS, [DeclarationFS])]

-- | reduceDependencies removes empty lists and merge multiple keys
reduceDependencies :: Dependencies -> Dependencies
reduceDependencies = map fromSet . Map.toList . Map.fromListWith (Set.union) . map toSet . filter isValuable
  where
    fromSet (k, v) = (k, Set.toList v)
    toSet (k, v) = (k, Set.fromList v)
    isValuable (_, []) = False
    isValuable (decl, _) = decl.declOccName `notElem` ["$trModule", "$krep"] && not (isTypeDecl decl.declOccName)
    isTypeDecl fs0 = isJust do
        (c1, fs1) <- unconsFS fs0
        (c2, fs2) <- unconsFS fs1
        (c3, _) <- unconsFS fs2
        if (c1, c2, c3) == ('$', 't', 'c')
            then Just ()
            else Nothing

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , driverPlugin = driverFun
        }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    return (CoreDoPluginPass "Collect Dependencies" pass : todo)

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

getDependenciesFromCoreBinds :: Module -> [CoreBind] -> Dependencies
getDependenciesFromCoreBinds genModule coreBinds =
    reduceDependencies $ foldMap (getDependenciesFromCore genModule) coreBinds

getDependenciesFromCore :: Module -> CoreBind -> Dependencies
getDependenciesFromCore genModule = \case
    NonRec b expr -> [(mkDecl genModule (varName b), getExprDeps expr)]
    Rec xs -> foldMap (getDependenciesFromCore genModule) ((\(b, e) -> NonRec b e) <$> xs)
  where
    getExprDeps :: Expr Var -> [DeclarationFS]
    getExprDeps = \case
        Var var -> [varDecl genModule var] -- this also capture local variable
        Lit _lit -> mempty
        App expr arg -> foldMap getExprDeps [expr, arg]
        Lam _b expr -> getExprDeps expr
        Let bind expr -> getBindDeps bind <> getExprDeps expr
        Case expr _b _type alt -> getExprDeps expr <> foldMap getAltDeps alt
        Cast expr _coer -> getExprDeps expr
        Tick _ expr -> getExprDeps expr
        Type _type -> mempty
        Coercion _coer -> mempty

    getBindDeps :: CoreBind -> [DeclarationFS]
    getBindDeps = \case
        NonRec _b expr -> getExprDeps expr
        Rec xs -> foldMap (\(_b, expr) -> getExprDeps expr) xs

    getAltDeps :: Alt Var -> [DeclarationFS]
    getAltDeps = \case
        Alt _altCon _bs expr -> getExprDeps expr

driverFun :: [CommandLineOption] -> HscEnv -> IO HscEnv
driverFun _ hscEnv = do
    let hooks =
            (hsc_hooks hscEnv)
                { runPhaseHook = Just (PhaseHook runPhaseFun)
                }
    pure $ hscEnv{hsc_hooks = hooks}

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

varDecl :: Module -> Var -> DeclarationFS
varDecl genModule var = case mkGlobalDecl name of
    Just decl -> decl
    Nothing -> mkDecl genModule name
  where
    name = varName var

mkGlobalDecl :: Name -> Maybe DeclarationFS
mkGlobalDecl name = do
    genModule <- nameModule_maybe name
    pure $ mkDecl genModule name

mkDecl :: Module -> Name -> DeclarationFS
mkDecl genModule name = DeclarationFS{declUnitId, declModuleName, declOccName, declUnique}
  where
    declUnitId = unitIdFS (moduleUnitId genModule)
    declModuleName = moduleNameFS (genModule.moduleName)
    declOccName = occNameFS (nameOccName name)
    declUnique = getKey (nameUnique name)

instance Binary.Binary DeclarationFS

instance Outputable DeclarationFS where
    ppr decl = hcat [ppr decl.declUnitId, ":", ppr decl.declModuleName, ".", ppr decl.declOccName, "_", ppr decl.declUnique]

instance Show DeclarationFS where
    show = showSDocOneLine defaultSDocContext . ppr

-- orphan
instance Ord FastString where
    compare = uniqCompareFS

-- orphan
instance Binary.Binary FastString where
    put fs = Binary.put (fastStringToShortByteString fs)
    get = mkFastStringShortByteString <$> Binary.get

printDependencies :: Dependencies -> IO ()
printDependencies dependencies = do
    forM_ dependencies \(decl, deps) -> do
        putStrLn $ show decl <> ": " <> intercalate ", " (show <$> deps)

writeDependencies :: FilePath -> Dependencies -> IO ()
writeDependencies fp dependencies = do
    putStrLn $ " XXX Writing " <> show (length dependencies) <> " dependencies info to: " <> fp
    Binary.encodeFile fp dependencies

readDependencies :: FilePath -> IO Dependencies
readDependencies fp = Binary.decodeFile fp
