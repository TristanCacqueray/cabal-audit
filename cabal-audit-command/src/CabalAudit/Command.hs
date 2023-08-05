module CabalAudit.Command where

import Control.Monad (forM_, unless)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (dropWhileEnd, group, intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import GHC.Unit.Module (ModuleName, mkModuleName, mkModuleNameFS)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OSP

import CabalAudit.GhcPkg
import CabalAudit.LoadUtils
import CabalAudit.Plugin

import Data.Maybe (listToMaybe)
import Options.Applicative
import System.IO (IOMode (..), hPutStrLn, stderr, withFile)

data Analysis = Analysis
    { callGraph :: Dependencies
    , missingModules :: Set ModuleName
    , unknownDeclsRef :: Set DeclarationFS
    }

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
    dupDecls :: Set FastString
    dupDecls = Set.fromList $ map head $ filter (\g -> length g > 1) $ group $ map (declOccName . fst) deps
    removeDeclUnique :: DeclarationFS -> DeclarationFS
    removeDeclUnique decl
        | decl.declOccName `Set.member` dupDecls = decl
        | otherwise = decl{declUnique = 0}
    removeTopDecl :: (DeclarationFS, [DeclarationFS]) -> (DeclarationFS, [DeclarationFS])
    removeTopDecl (decl, declDeps) = (removeDeclUnique decl, map removeDeclUnique declDeps)

collectDependencies :: [OsPath] -> [ModuleName] -> IO Analysis
collectDependencies rootPaths rootModules = do
    -- Collect issues
    missingModules <- newIORef mempty
    unknownDecls <- newIORef mempty
    let addEmpty ref obj = do
            modifyIORef ref $ Set.insert obj
            pure []

    -- Logic to read .hix files
    allModules <- findModulesPath [osp|.hix|] rootPaths
    loadedDependencies :: IORef (Map ModuleName Dependencies) <- newIORef mempty
    let readModuleDependencies :: ModuleName -> IO Dependencies
        readModuleDependencies moduleName =
            Map.lookup moduleName <$> readIORef loadedDependencies >>= \case
                Just deps -> pure deps
                Nothing -> do
                    deps <-
                        removeUnique <$> case Map.lookup moduleName allModules of
                            Nothing -> addEmpty missingModules moduleName
                            Just fp -> readDependencies =<< OSP.decodeFS fp
                    modifyIORef loadedDependencies $ Map.insert moduleName deps
                    -- printDependencies deps
                    pure deps

    -- Logic to find dependencies
    rootDependencies <- Map.fromList <$> foldMap readModuleDependencies rootModules
    allDependencies :: IORef (Map DeclarationFS [DeclarationFS]) <- newIORef rootDependencies
    let go :: DeclarationFS -> IO ()
        go decl =
            Map.lookup decl <$> readIORef allDependencies >>= \case
                Just _ -> pure ()
                Nothing -> do
                    moduleDecls <- readModuleDependencies (mkModuleNameFS decl.declModuleName)
                    declDeps <- case lookup decl moduleDecls of
                        Nothing -> addEmpty unknownDecls decl
                        Just newDependencies -> pure newDependencies
                    modifyIORef allDependencies $ Map.insert decl declDeps
                    -- collect the dependencies of the dependencies
                    traverse_ go declDeps
    traverse_ go (concat $ Map.elems rootDependencies)

    pure Analysis
        <*> (Map.toList <$> readIORef allDependencies)
        <*> readIORef missingModules
        <*> readIORef unknownDecls

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
        Just decl -> do
            hPutStrLn stderr $ show decl <> ": call by TODO"
        Nothing -> hPutStrLn stderr $ target <> ": couldn't find in the dependencies " <> show (length callGraph)

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

main :: IO ()
main = do
    args <- execParser opts
    libs <- getLibraryDirs

    let rootPaths = [osp|./dist-newstyle|] : args.extraLibDirs <> Set.toList libs
    analysis <- collectDependencies rootPaths args.rootModules
    unless (Set.null analysis.missingModules) do
        hPutStrLn stderr $ "Unknown modules: " <> intercalate ", " (show <$> Set.toList analysis.missingModules)
    unless (Set.null analysis.unknownDeclsRef) do
        hPutStrLn stderr $ "Unknown declaration: " <> intercalate ", " (show <$> Set.toList analysis.unknownDeclsRef)

    forM_ args.graphOutput (dumpGraph analysis.callGraph)

    case args.targetDecls of
        [] ->
            -- TODO: load the vulnerable target from the advisory db
            dumpDependencies analysis.callGraph
        xs -> checkTarget analysis.callGraph xs
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

    opts =
        info
            (usage <**> helper)
            ( fullDesc
                <> header "cabal-audit - detects uses of known vulnerabilities"
            )
