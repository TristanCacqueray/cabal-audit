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

import Options.Applicative
import System.IO (IOMode (..), hPutStrLn, stderr, withFile)

data Analysis = Analysis
    { callGraph :: Dependencies
    , missingModules :: Set ModuleName
    , unknownDeclsRef :: Set DeclarationFS
    }

-- | Unique are not stable accross module, so we remove them here until the plugin generate them correctly
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
                    traverse_ go declDeps
    traverse_ go (concat $ Map.elems rootDependencies)

    pure Analysis
        <*> (Map.toList <$> readIORef allDependencies)
        <*> readIORef missingModules
        <*> readIORef unknownDecls

vulnToDecl :: Dependencies -> String -> Maybe DeclarationFS
vulnToDecl callGraph vuln = findDecl callGraph
  where
    findDecl [] = Nothing
    findDecl ((decl, _) : rest)
        | declMatch decl = Just decl
        | otherwise = findDecl rest
    declMatch :: DeclarationFS -> Bool
    declMatch decl = vulnModule == decl.declModuleName && vulnName == decl.declOccName

    vulnModule = mkFastString $ dropWhileEnd (/= '.') vuln
    vulnDecl = reverse $ takeWhile (/= '.') $ reverse vuln
    vulnName = mkFastString $ vulnDecl

checkVuln :: [String] -> Dependencies -> IO ()
checkVuln _vulns _callGraph = do
    pure ()

data CabalAuditUsage = CabalAuditUsage
    { extraLibDirs :: [OsPath]
    , vulnDecls :: [String]
    , rootModules :: [ModuleName]
    }

usage :: Parser CabalAuditUsage
usage =
    pure CabalAuditUsage
        <*> many (toOsPath <$> strOption (long "extra-lib-dirs"))
        <*> many (strOption (long "vuln" <> help "test"))
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
        hPutStrLn stderr $ "Unknown modules: " <> intercalate "," (show <$> Set.toList analysis.missingModules)
    unless (Set.null analysis.unknownDeclsRef) do
        hPutStrLn stderr $ "Unknown declaration: " <> intercalate "," (show <$> Set.toList analysis.unknownDeclsRef)

    case args.vulnDecls of
        [] -> dumpGraph analysis.callGraph
        xs -> checkVuln xs analysis.callGraph
  where
    dumpGraph callGraph = do
        withFile "edges.tsv" WriteMode \handle -> do
            hPutStrLn handle $ intercalate "\t" ["Source", "Target"]
            forM_ callGraph \(sourceDecl, deps) -> do
                forM_ deps \targetDecl -> do
                    hPutStrLn handle $ intercalate "\t" [show sourceDecl, show targetDecl]
        withFile "nodes.tsv" WriteMode \handle -> do
            hPutStrLn handle $ intercalate "\t" ["Id", "Label"]
            forM_ callGraph \(decl, _) -> do
                hPutStrLn handle $ intercalate "\t" [show decl, declFmt decl]

    opts =
        info
            (usage <**> helper)
            ( fullDesc
                <> header "cabal-audit - detects uses of known vulnerabilities"
            )
