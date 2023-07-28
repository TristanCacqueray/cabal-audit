module CabalAudit where

-- base
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.List (intersperse)
import Control.Applicative ((<|>))

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
import GHC.Utils.Outputable (Outputable (ppr), SDoc, defaultSDocContext, showSDocOneLine)
import GHC.Unit.Module
import GHC.Utils.Outputable (hcat)

-- cabal-audit
import HieLoader

data Declaration = Declaration
    { declModule :: Module
    , declOccName :: OccName
    }
    deriving (Ord, Eq)

data Analysis = Analysis
    { dependencyGraph :: Graph Declaration
    , roots :: Set Declaration
    }
    deriving (Generic)

main :: IO ()
main = do
    let hieLocations = ["./dist-newstyle"]
        exposedModule = mkModuleName <$> ["CabalAudit", "HieLoader"]
    analysis <- listExternalNames hieLocations exposedModule
    printExternalNames analysis

printExternalNames :: Analysis -> IO ()
printExternalNames analysis = evalStateT go mempty
  where
    go :: StateT (Set Declaration) IO ()
    go = traverse_ goRoot analysis.roots

    goRoot :: Declaration -> StateT (Set Declaration) IO ()
    goRoot decl = do
        let reachable :: [Declaration]
            reachable = Graph.reachable decl analysis.dependencyGraph
        known <- get
        case filter (`notElem` known) (filter (/= decl) reachable) of
            [] -> pure ()
            xs -> do
                lift $ putStr (show decl <> ": ")
                traverse_ (lift . putStr) (intersperse ", " (show <$> xs))
                put $ Set.union known (Set.fromList reachable)
                lift $ putStr "\n"

listExternalNames :: [FilePath] -> [ModuleName] -> IO Analysis
listExternalNames hiePaths rootModules = do
    hieState <- newHieState hiePaths
    let analysis = Analysis mempty mempty
    execStateT (doListExternalNames hieState rootModules) analysis

doListExternalNames :: HieState -> [ModuleName] -> StateT Analysis IO ()
doListExternalNames hieState rootModules = do
    forM_ rootModules \rootModule -> do
        lift (lookupOrLoadHieFile hieState rootModule) >>= \case
            Nothing -> lift (putStrLn $ "No hie file for " <> show rootModule)
            Just hieFile -> do
                forM_ (getDependencies hieFile) \dependency -> do
                    case dependency.parent of
                        Nothing -> error "this is impossible!"
                        Just decl -> do
                            #roots %= Set.insert decl
                            #dependencyGraph %= overlay (edge decl dependency.decl)

        -- todo: load external hie file and dive in the dependency tree
        pure ()

data Dependency = Dependency
    { decl :: Declaration
    , ctxInfo :: Set ContextInfo
    , parent :: Maybe Declaration
    }

instance Show Dependency where
    show extName =
        showPpr $
            hcat [ppr extName.parent, " -> ", ppr extName.decl, " ", ppr extName.ctxInfo]

showPpr :: SDoc -> String
showPpr = showSDocOneLine defaultSDocContext

instance Outputable Declaration where
    ppr decl = hcat [ppr decl.declModule, ".", ppr decl.declOccName]

instance Show Declaration where
    show decl = showPpr (ppr decl)

isToplevelDeclaration :: Dependency -> Maybe Declaration
isToplevelDeclaration extName =
    Set.foldr
        ( \ctx acc -> case acc of
            Nothing -> isTopDecl ctx
            _ -> acc
        )
        Nothing
        extName.ctxInfo
  where
    isTopDecl :: ContextInfo -> Maybe Declaration
    isTopDecl = \case
        ValBind _bindType ModuleScope _span -> Just extName.decl
        _ -> Nothing

isUsage :: Dependency -> Bool
isUsage extName = isJust extName.parent && Use `Set.member` extName.ctxInfo

getDependencies :: HieFile -> [Dependency]
getDependencies hieFile = filter isUsage $ doGet 0 Nothing (Map.elems hieFile.hie_asts.getAsts)
  where
    doGet :: Int -> Maybe Declaration -> [HieAST TypeIndex] -> [Dependency]
    doGet _depth _mParent [] = []
    doGet depth mParent (lhs : rest) = lhsName <> concatMap (doGetName depth newParent) rest
      where
        lhsName = doGetName depth mParent lhs
        lhsParent = listToMaybe $ mapMaybe isToplevelDeclaration lhsName
        newParent = mParent <|> lhsParent

    doGetName :: Int -> Maybe Declaration -> HieAST TypeIndex -> [Dependency]
    doGetName depth mParent hieAST = doGetNode hieAST.sourcedNodeInfo <> doGet (depth + 1) mParent hieAST.nodeChildren
      where
        doGetNode :: SourcedNodeInfo TypeIndex -> [Dependency]
        doGetNode node = concatMap doGetNodeInfo (Map.elems node.getSourcedNodeInfo)

        doGetNodeInfo :: NodeInfo TypeIndex -> [Dependency]
        doGetNodeInfo nodeInfo = concatMap doGetNodeIdentifiers (Map.toList nodeInfo.nodeIdentifiers)

        doGetNodeIdentifiers :: (Identifier, IdentifierDetails TypeIndex) -> [Dependency]
        doGetNodeIdentifiers (identifier, identifierDetails) = case identifier of
            Left _moduleName -> []
            Right name -> case nameToDeclaration name of
                Just decl -> [Dependency decl identifierDetails.identInfo mParent]
                _ -> []

nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
    m <- nameModule_maybe name
    return Declaration{declModule = m, declOccName = nameOccName name}
