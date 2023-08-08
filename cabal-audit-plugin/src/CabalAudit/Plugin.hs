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
import Data.Set (Set)
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
    , declUnique :: Int -- current solution to differentiate class instance
    }
    deriving (Eq, Generic, Ord)

-- Q: How to tell two class instance appart?
-- A: Use the var name Unique?

-- | Dependencies (aka the call graph)
type Dependencies = [(DeclarationFS, [DeclarationFS])]

-- | reduceDependencies tidy up the output a bit.
reduceDependencies :: Dependencies -> Dependencies
reduceDependencies = combine . filter isValuable
  where
    -- Remove duplicate edge and merge top level nodes
    combine = map fromSet . Map.toList . Map.fromListWith (Set.union) . map toSet
    fromSet (k, v) = (k, Set.toList v)
    toSet (k, v) = (k, Set.fromList v)
    isValuable (decl, _) =
        -- Ignore useless vars
        decl.declOccName `notElem` ["$krep"] && not (isTypeDecl decl.declOccName)
    -- var that starts with '$tc' and '$tr' doesn't seem relevant
    isTypeDecl fs0 = isJust do
        (c1, fs1) <- unconsFS fs0
        (c2, fs2) <- unconsFS fs1
        (c3, _) <- unconsFS fs2
        if (c1, c2, c3) == ('$', 't', 'c') || (c1, c2, c3) == ('$', 't', 'r')
            then Just ()
            else Nothing

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

getDependenciesFromCoreBinds :: Module -> [CoreBind] -> Dependencies
getDependenciesFromCoreBinds genModule coreBinds =
    reduceDependencies $ foldMap (getDependenciesFromCore genModule topVars) coreBinds
  where
    topVars = getTopVars coreBinds

-- | Collect all the top level Vars.
getTopVars :: [CoreBind] -> Set Var
getTopVars = go mempty
  where
    go acc [] = acc
    go acc (x : rest) = case x of
        NonRec b _ -> go (Set.insert b acc) rest
        Rec recs ->
            let recVars = Set.fromList (map fst recs)
             in go (Set.union recVars acc) rest

getDependenciesFromCore :: Module -> Set Var -> CoreBind -> Dependencies
getDependenciesFromCore genModule topVars coreBind = case coreBind of
    NonRec b expr -> [(mkDecl genModule (varName b), getExprDeps expr)]
    Rec xs -> foldMap (getDependenciesFromCore genModule topVars) ((\(b, e) -> NonRec b e) <$> xs)
  where
    -- Check if a variable comes from an external module
    isExternalVar :: Var -> Bool
    isExternalVar var = case nameModule_maybe (varName var) of
        Just varGenModule -> varGenModule /= genModule
        Nothing -> False

    getExprDeps :: Expr Var -> [DeclarationFS]
    getExprDeps = \case
        Var var
            | -- Only track external or top level vars
              isExternalVar var || var `Set.member` topVars ->
                [varDecl genModule var]
            | -- And ignore local or shadow vars
              otherwise ->
                []
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

-- | Create a node for the call graph.
mkDecl :: Module -> Name -> DeclarationFS
mkDecl genModule name = DeclarationFS{declUnitId, declModuleName, declOccName, declUnique}
  where
    declUnitId = unitIdFS (moduleUnitId genModule)
    declModuleName = moduleNameFS (genModule.moduleName)
    declOccName = occNameFS (nameOccName name)
    declUnique = getKey (nameUnique name)

instance Binary.Binary DeclarationFS

instance Outputable DeclarationFS where
    ppr decl =
        let uniqStr = case decl.declUnique of
                0 -> ""
                n -> hcat ["_", ppr n]
         in hcat [ppr decl.declUnitId, ":", ppr decl.declModuleName, ".", ppr decl.declOccName, uniqStr]

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
