module CabalAudit.Core (
    getDependenciesFromCoreBinds,
    DeclarationFS (..),
    Dependencies,
) where

import Data.Binary qualified as Binary
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Core (Alt (..), Bind (..), CoreBind, Expr (..))
import GHC.Core.Type (Var)
import GHC.Data.FastString (FastString, NonDetFastString (..), fastStringToShortByteString, mkFastStringShortByteString, unconsFS)
import GHC.Generics (Generic)
import GHC.Types.Name (Name, OccName (occNameFS), nameModule_maybe, nameOccName)
import GHC.Types.Var (varName)
import GHC.Unit (moduleNameFS)
import GHC.Unit.Module (Module, moduleUnitId)
import GHC.Unit.Types (GenModule (moduleName), UnitId (..))
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, hcat, showSDocOneLine)

data DeclarationFS = DeclarationFS
    { declModuleName :: FastString
    , declUnitId :: FastString
    , declOccName :: FastString
    }
    deriving (Eq)

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
        decl.declOccName `notElem` ["$krep"] && not (isNameIgnored decl.declOccName)
    -- var that starts with '$tc' and '$tr' doesn't seem relevant
    isNameIgnored fs0 = isJust do
        (c1, fs1) <- unconsFS fs0
        (c2, fs2) <- unconsFS fs1
        (c3, _) <- unconsFS fs2
        if (c1, c2, c3) == ('$', 't', 'c') || (c1, c2, c3) == ('$', 't', 'r')
            then Just ()
            else Nothing

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

-- | Process a 'CoreBind' into a 'Dependencies'
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
mkDecl genModule name = DeclarationFS{declUnitId, declModuleName, declOccName}
  where
    declUnitId = unitIdFS (moduleUnitId genModule)
    declModuleName = moduleNameFS (moduleName genModule)
    declOccName = occNameFS (nameOccName name)

instance Outputable DeclarationFS where
    ppr decl =
        hcat [ppr decl.declUnitId, ":", ppr decl.declModuleName, ".", ppr decl.declOccName]

instance Show DeclarationFS where
    show = showSDocOneLine defaultSDocContext . ppr

instance Binary.Binary DeclarationFS where
    put (DeclarationFS declModuleName declUnitId declOccName) = do
        putFS declModuleName
        putFS declUnitId
        putFS declOccName
      where
        putFS = Binary.put . fastStringToShortByteString
    get = DeclarationFS <$> getFS <*> getFS <*> getFS
      where
        getFS = mkFastStringShortByteString <$> Binary.get

instance Ord DeclarationFS where
    compare d1 d2 =
        fscomp d1.declModuleName d2.declModuleName
            <> fscomp d1.declUnitId d2.declUnitId
            <> fscomp d1.declOccName d2.declOccName
      where
        fscomp f1 f2 = compare (NonDetFastString f1) (NonDetFastString f2)
