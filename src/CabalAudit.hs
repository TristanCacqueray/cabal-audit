module CabalAudit where

-- base
import Control.Monad
import Data.Coerce (coerce)
import Data.Foldable
import Data.List (intersperse)
import Data.Maybe

-- containers
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- generic-lens
import Data.Generics.Labels ()

-- lens
import Control.Lens ((%=))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- algebraic-graphs
import Algebra.Graph (Graph, edge, overlay)
import Algebra.Graph.ToGraph qualified as Graph

-- ghc
import GHC.Generics (Generic)
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Utils.Outputable hiding ((<>))

-- cabal-audit
import HieLoader

-- | A symbol that is imported
data Declaration = Declaration
    { declModule :: Module
    , declOccName :: OccName
    }
    deriving (Ord, Eq)

-- | A symbol that is defined
newtype TopLevelDeclaration = TopLevelDeclaration Declaration
    deriving newtype (Ord, Eq, Show)

data Analysis = Analysis
    { dependencyGraph :: Graph Declaration
    , roots :: Set TopLevelDeclaration
    }
    deriving (Generic)

main :: IO ()
main = do
    let hieLocations = ["./dist-newstyle"]
        exposedModule = mkModuleName <$> ["CabalAudit.Test.Simple", "CabalAudit.Test.User", "CabalAudit.Test.Instance"]
    analysis <- doAnalyze hieLocations exposedModule
    printExternalNames analysis

-- | A helper to dump the analysis.
printExternalNames :: Analysis -> IO ()
printExternalNames analysis = evalStateT go mempty
  where
    go :: StateT (Set Declaration) IO ()
    go = traverse_ goRoot analysis.roots

    goRoot :: TopLevelDeclaration -> StateT (Set Declaration) IO ()
    goRoot decl = do
        let reachables :: [Declaration]
            reachables = Graph.reachable (coerce decl) analysis.dependencyGraph
        known <- get
        case filter (`notElem` known) (filter (/= coerce decl) reachables) of
            [] -> pure ()
            decls -> do
                lift $ putStr (show decl <> ": ")
                traverse_ (lift . putStr) (intersperse ", " (show <$> decls))
                put $ Set.union known (Set.fromList decls)
                lift $ putStr "\n"

-- | Perform the analysis
doAnalyze :: [FilePath] -> [ModuleName] -> IO Analysis
doAnalyze hiePaths rootModules = do
    hieState <- newHieState hiePaths
    let analysis = Analysis mempty mempty
    flip execStateT analysis do
        forM_ rootModules \rootModule -> do
            lift (lookupOrLoadHieFile hieState rootModule) >>= \case
                Nothing -> lift (putStrLn $ "No hie file for " <> show rootModule)
                Just hieFile -> do
                    forM_ (getDependencies hieFile) \(topLevelDecl, decl) -> do
                        #roots %= Set.insert (coerce topLevelDecl)
                        #dependencyGraph %= overlay (edge (coerce topLevelDecl) decl)

        -- todo: load external hie file and dive in the dependency tree
        pure ()

data DeclarationInfo = DeclarationInfo
    { decl :: Declaration
    , ctxInfo :: Set ContextInfo
    }

instance Show DeclarationInfo where
    show extName = showPpr $ hcat $ [ppr extName.decl, " ", ppr extName.ctxInfo]

showPpr :: SDoc -> String
showPpr = showSDocOneLine defaultSDocContext

instance Outputable Declaration where
    ppr decl = hcat [ppr decl.declModule, ".", ppr decl.declOccName]

instance Show Declaration where
    show decl = showPpr (ppr decl)

-- | Check if a declaration is a top level bind
isToplevelDeclaration :: DeclarationInfo -> Maybe TopLevelDeclaration
isToplevelDeclaration extName =
    Set.foldr
        ( \ctx acc -> case acc of
            Nothing -> isTopDecl ctx
            _ -> acc
        )
        Nothing
        extName.ctxInfo
  where
    isTopDecl :: ContextInfo -> Maybe TopLevelDeclaration
    isTopDecl = \case
        ValBind _bindType ModuleScope _span -> Just (TopLevelDeclaration extName.decl)
        _ -> Nothing

isUsage :: DeclarationInfo -> Bool
isUsage extName = Use `Set.member` extName.ctxInfo

-- | Returns all the edges between a top level declaration and its dependency.
getDependencies :: HieFile -> [(TopLevelDeclaration, Declaration)]
getDependencies hieFile =
    map (fmap (.decl)) $ filter (isUsage . snd) $ doGet Nothing (Map.elems hieFile.hie_asts.getAsts)
  where
    doGet :: Maybe TopLevelDeclaration -> [HieAST TypeIndex] -> [(TopLevelDeclaration, DeclarationInfo)]
    doGet _mLHS [] = []
    doGet Nothing (cur : rest) = doGet mLHS cur.nodeChildren <> doGet mLHS rest
      where
        -- Is the current node a left-hand-side binder?
        mLHS :: Maybe TopLevelDeclaration
        mLHS = listToMaybe $ mapMaybe isToplevelDeclaration curNodeInfo
        curNodeInfo = concatMap (doGetNode . (.sourcedNodeInfo)) (cur : cur.nodeChildren)
    doGet (Just decl) xs = (\di -> (decl, di)) <$> concatMap (doGetName decl) xs

    doGetName :: TopLevelDeclaration -> HieAST TypeIndex -> [DeclarationInfo]
    doGetName lhs hieAST = doGetNode hieAST.sourcedNodeInfo <> fmap snd (doGet (Just lhs) hieAST.nodeChildren)

    doGetNode :: SourcedNodeInfo TypeIndex -> [DeclarationInfo]
    doGetNode node = concatMap doGetNodeInfo (Map.elems node.getSourcedNodeInfo)

    doGetNodeInfo :: NodeInfo TypeIndex -> [DeclarationInfo]
    doGetNodeInfo nodeInfo = concatMap doGetNodeIdentifiers (Map.toList nodeInfo.nodeIdentifiers)

    doGetNodeIdentifiers :: (Identifier, IdentifierDetails TypeIndex) -> [DeclarationInfo]
    doGetNodeIdentifiers (identifier, identifierDetails) = case identifierToDeclaration identifier of
        Just decl -> [DeclarationInfo decl identifierDetails.identInfo]
        Nothing -> []

identifierToDeclaration :: Identifier -> Maybe Declaration
identifierToDeclaration = \case
    Left _moduleName -> Nothing
    Right name -> nameToDeclaration name

nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
    m <- nameModule_maybe name
    pure Declaration{declModule = m, declOccName = nameOccName name}
