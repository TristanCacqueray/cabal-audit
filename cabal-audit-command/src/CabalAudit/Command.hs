module CabalAudit.Command where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class
import Data.Binary qualified as Binary
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, group, intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import GHC.Data.FastString (NonDetFastString (..), mkFastString, unpackFS)
import GHC.Unit.Module (ModuleName, mkModuleName, mkModuleNameFS)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OSP

import CabalAudit.Analysis
import CabalAudit.Core
import CabalAudit.GhcPkg
import CabalAudit.LoadUtils

import Control.Monad.Trans.State.Strict (StateT)
import Data.Maybe (listToMaybe)
import Options.Applicative
import System.IO (IOMode (..), hPutStrLn, stderr, withFile)

{- | This functions keeps the unique attribute only for duplicate top level declaration.
 This is necessary because Unique are not stable accross module.
 The assumptions is that:
 1. only type class instance (starting with $c) needs unique (their toplevel name appears dupplicated otherwise).
 2. These $c variables are not used directly, external users reference the '$f' variant instead.
 Therefor it should be safe to remove the Unique for non duplicated declaration.
 However this seems like a hack to get the relevant type class instance declaration.
-}
removeUnique :: Dependencies -> Dependencies
removeUnique deps = map removeTopDecl deps
  where
    dupDecls :: Set NonDetFastString
    dupDecls = Set.fromList $ map head $ filter (\g -> length g > 1) $ group $ map (NonDetFastString . declOccName . fst) deps
    removeDeclUnique :: DeclarationFS -> DeclarationFS
    removeDeclUnique decl
        | NonDetFastString decl.declOccName `Set.member` dupDecls = decl
        | otherwise = decl{declUnique = 0}
    removeTopDecl :: (DeclarationFS, [DeclarationFS]) -> (DeclarationFS, [DeclarationFS])
    removeTopDecl (decl, declDeps) = (removeDeclUnique decl, map removeDeclUnique declDeps)

collectDependencies :: [OsPath] -> [ModuleName] -> IO Analysis
collectDependencies rootPaths rootModules = runAnalysis do
    -- Logic to read .hix files
    allModules <- liftIO (findModulesPath [osp|.hix|] rootPaths)
    let readModuleDependencies :: ModuleName -> StateT Analysis IO Dependencies
        readModuleDependencies moduleName = do
            let moduleInfo = ModuleFS moduleName Nothing -- .hix doesn't contain the unitId
            lookupOrLoadModule moduleInfo $ case Map.lookup moduleName allModules of
                Just fp -> Just <$> (Binary.decodeFile =<< OSP.decodeFS fp)
                Nothing -> pure Nothing

    -- Logic to find dependencies
    rootDependencies <- Map.fromList . concat <$> traverse readModuleDependencies rootModules
    setRootDeclarations rootDependencies
    let go :: DeclarationFS -> StateT Analysis IO ()
        go decl =
            Map.lookup decl <$> getAnalysis callGraph >>= \case
                Just _ -> pure () -- Already processed
                Nothing -> do
                    moduleDecls <- readModuleDependencies (mkModuleNameFS decl.declModuleName)
                    declDeps <- case lookup decl moduleDecls of
                        Nothing -> do
                            addUnknownDecl decl
                            pure []
                        Just newDependencies -> pure newDependencies
                    addDeclaration decl declDeps
                    -- collect the dependencies of the dependencies
                    traverse_ go declDeps
    traverse_ go (concat $ Map.elems rootDependencies)

-- | Convert the user-provided dependency definition into a DeclarationFS
findTarget :: Dependencies -> String -> Maybe DeclarationFS
findTarget callGraph target = findDecl callGraph
  where
    findDecl [] = Nothing
    findDecl ((decl, decls) : rest)
        | declMatch decl = Just decl
        | otherwise = listToMaybe (filter declMatch decls) <|> findDecl rest
    declMatch :: DeclarationFS -> Bool
    declMatch decl = targetModule == decl.declModuleName && targetName == decl.declOccName

    targetModule = mkFastString $ dropWhileEnd (== '.') $ dropWhileEnd (/= '.') target
    targetDecl = reverse $ takeWhile (/= '.') $ reverse target
    targetName = mkFastString $ targetDecl

checkTarget :: Dependencies -> [String] -> IO ()
checkTarget callGraph targets = forM_ targets \target -> do
    case findTarget callGraph target of
        Just decl -> hPutStrLn stderr $ Tree.drawTree (show <$> getPath decl)
        Nothing -> hPutStrLn stderr $ target <> ": couldn't find in the dependencies " <> show (length callGraph)
  where
    getPath :: DeclarationFS -> Tree DeclarationFS
    getPath root = Tree.Node root (getChilds [] $ maybe mempty Set.toList (Map.lookup root inverseGraph))
      where
        getChilds acc [] = acc
        getChilds acc (cur : rest) = getChilds (getPath cur : acc) rest

    inverseGraph :: Map DeclarationFS (Set DeclarationFS)
    inverseGraph = Map.fromListWith Set.union $ concatMap toInverse callGraph
    toInverse :: (DeclarationFS, [DeclarationFS]) -> [(DeclarationFS, Set DeclarationFS)]
    toInverse (source, targets') = go [] targets'
      where
        go acc [] = acc
        go acc (target : rest) = go ((target, Set.singleton source) : acc) rest

data CabalAuditUsage = CabalAuditUsage
    { extraLibDirs :: [OsPath]
    , graphOutput :: Maybe FilePath
    , targetDecls :: [String]
    , rootModules :: [ModuleName]
    }

usage :: Parser CabalAuditUsage
usage =
    pure CabalAuditUsage
        <*> many (toOsPath <$> strOption (long "extra-lib-dirs" <> metavar "DIR" <> help "Search module dependencies in DIR (e.g. for ghc librarires)"))
        <*> optional (strOption (long "write-graph" <> metavar "FILENAME" <> help "Dump nodes.tsv and edges.tsv files"))
        <*> many (strOption (long "target" <> metavar "DECLARATION" <> help "Check if a declaration is reachable"))
        <*> some (mkModuleName <$> argument str (metavar "MODULE..."))
  where
    toOsPath :: FilePath -> OsPath
    toOsPath fp = case OSP.encodeUtf fp of
        Nothing -> error $ fp <> ": bad fp"
        Just ofp -> ofp

declFmt :: DeclarationFS -> String
declFmt decl = unpackFS decl.declModuleName <> "." <> unpackFS decl.declOccName

getArgs :: IO CabalAuditUsage
getArgs = execParser opts
  where
    opts =
        info
            (usage <**> helper)
            ( fullDesc
                <> header "cabal-audit - detects uses of known vulnerabilities"
            )

main :: IO ()
main = do
    args <- getArgs
    libs <- getLibraryDirs

    let rootPaths = [osp|./dist-newstyle|] : args.extraLibDirs <> Set.toList libs
    analysis <- collectDependencies rootPaths args.rootModules
    checkAnalysis args analysis

checkAnalysis :: CabalAuditUsage -> Analysis -> IO ()
checkAnalysis args analysis = do
    unless (Set.null analysis.missingModules) do
        hPutStrLn stderr $ "Unknown modules: " <> intercalate ", " (show <$> Set.toList analysis.missingModules)
    unless (Set.null analysis.unknownDecls) do
        hPutStrLn stderr $ "Unknown declaration: " <> intercalate ", " (show <$> Set.toList analysis.unknownDecls)

    forM_ args.graphOutput (dumpGraph $ Map.toList analysis.callGraph)

    case args.targetDecls of
        [] ->
            -- TODO: load the vulnerable target from the advisory db
            dumpDependencies (Map.toList analysis.callGraph)
        xs -> checkTarget (Map.toList analysis.callGraph) xs
  where
    dumpDependencies callGraph = do
        forM_ callGraph \(decl, deps) -> do
            unless (null deps) do
                putStrLn $ show decl <> ": " <> intercalate ", " (show <$> deps)

    dumpGraph callGraph fp = do
        withFile (fp <> "-edges.tsv") WriteMode \handle -> do
            hPutStrLn handle $ intercalate "\t" ["Source", "Target"]
            forM_ callGraph \(sourceDecl, deps) -> do
                forM_ deps \targetDecl -> do
                    hPutStrLn handle $ intercalate "\t" [show sourceDecl, show targetDecl]
        withFile (fp <> "-nodes.tsv") WriteMode \handle -> do
            hPutStrLn handle $ intercalate "\t" ["Id", "Label"]
            forM_ callGraph \(decl, _) -> do
                hPutStrLn handle $ intercalate "\t" [show decl, declFmt decl]
