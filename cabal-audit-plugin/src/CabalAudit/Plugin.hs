{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CabalAudit.Plugin (
    plugin,
    DeclarationFS (..),
    Dependencies,
    printDependencies,
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

data DeclarationFS = DeclarationFS
    { declModuleName :: FastString
    , declUnitId :: FastString
    , declOccName :: FastString
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
    return (todo <> [CoreDoPluginPass "Collect Dependencies" pass])

{-# NOINLINE globalEnvIORef #-}
globalEnvIORef :: IORef Dependencies
globalEnvIORef = unsafePerformIO $ newIORef mempty

-- | Pass collects the dependencies and register them to the global env for the driverFun
pass :: ModGuts -> CoreM ModGuts
pass guts = do
    let genModule = guts.mg_module
    let getDependencies :: CoreBind -> Dependencies
        getDependencies = \case
            NonRec b expr -> [(mkDecl genModule (varName b), getExprDeps expr)]
            Rec xs -> foldMap getDependencies ((\(b, e) -> NonRec b e) <$> xs)

        getExprDeps :: Expr Var -> [DeclarationFS]
        getExprDeps = \case
            Var var -> [varDecl genModule var]
            Lit _lit -> mempty
            App expr arg -> foldMap getExprDeps [expr, arg]
            Lam b expr -> varDecl genModule b : getExprDeps expr
            Let bind expr -> getBindDeps bind <> getExprDeps expr
            Case expr b _type alt -> varDecl genModule b : getExprDeps expr <> foldMap getAltDeps alt
            Cast expr _coer -> getExprDeps expr
            Tick _ expr -> getExprDeps expr
            Type _type -> mempty
            Coercion _coer -> mempty

        getBindDeps :: CoreBind -> [DeclarationFS]
        getBindDeps = \case
            NonRec b expr -> varDecl genModule b : getExprDeps expr
            Rec xs -> foldMap (\(b, expr) -> varDecl genModule b : getExprDeps expr) xs

        getAltDeps :: Alt Var -> [DeclarationFS]
        getAltDeps = \case
            Alt _altCon bs expr -> map (varDecl genModule) bs <> getExprDeps expr

    let dependencies :: Dependencies
        dependencies = reduceDependencies $ foldMap getDependencies guts.mg_binds

    -- liftIO $ printDependencies dependencies
    liftIO $ writeIORef globalEnvIORef dependencies
    pure guts

driverFun :: [CommandLineOption] -> HscEnv -> IO HscEnv
driverFun _ hscEnv = do
    let hooks =
            (hsc_hooks hscEnv)
                { runPhaseHook = Just (PhaseHook runPhaseFun)
                }
    pure $ hscEnv{hsc_hooks = hooks}

-- | Serialize the collected dependencies (from the global env) next to the .hi file
runPhaseFun :: TPhase a -> IO a
runPhaseFun phase = case phase of
    -- T_Cc _phase _pipeEnv hscEnv location input_fn -> do
    T_HscBackend _pipeEnv _hscEnv _modName _hscSource modLocation _action -> do
        result <- runPhase phase
        dependencies <- readIORef globalEnvIORef
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
mkDecl genModule name = DeclarationFS{declUnitId, declModuleName, declOccName}
  where
    declUnitId = unitIdFS (moduleUnitId genModule)
    declModuleName = moduleNameFS (genModule.moduleName)
    declOccName = occNameFS (nameOccName name)

instance Binary.Binary DeclarationFS

instance Outputable DeclarationFS where
    ppr decl = hcat [ppr decl.declUnitId, ":", ppr decl.declModuleName, ".", ppr decl.declOccName]

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
    putStrLn $ "XXX Writing dependencies info " <> fp <> ": " <> show (length dependencies)
    Binary.encodeFile fp dependencies
