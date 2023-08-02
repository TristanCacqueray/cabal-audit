module HieLoader (
    -- * Hie paths
    loadHiePaths,
    HiePaths,

    -- * Hie loader
    newHieState,
    lookupOrLoadHieFile,
    HieState,
)
where

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache

import Control.Concurrent.MVar
import Data.Char (isUpper)
import Data.List (dropWhileEnd, intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Unit.Module
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory)
import System.FilePath (splitExtensions, splitPath)

{- | Get module name from a hie path

 >>> isHiePath "./dist-newstyle/build/Riri/Fifi/Loulou.hie"
 Just (ModuleName "Riri.Fifi.Loulou")
-}
isHiePath :: FilePath -> Maybe ModuleName
isHiePath basePath = case splitExtensions basePath of
    (fp, ext)
        | ext == ".hie" -> Just $ mkModuleName (toModuleName fp)
        | otherwise -> Nothing
  where
    toModuleName :: FilePath -> String
    toModuleName = intercalate "." . reverse . takeWhile isModule . reverse . map (dropWhileEnd (== '/')) . splitPath
    isModule :: FilePath -> Bool
    isModule = \case
        (c : _) | isUpper c -> True
        _ -> False

newtype HiePaths = HiePaths (Map ModuleName FilePath)
    deriving (Show)

loadHiePaths :: [FilePath] -> IO HiePaths
loadHiePaths paths = HiePaths . Map.fromList . concat <$> traverse go paths
  where
    go :: FilePath -> IO [(ModuleName, FilePath)]
    go path = do
        exists <- doesPathExist path
        if exists
            then do
                isFile <- doesFileExist path
                if isFile
                    then goFile <$> canonicalizePath path
                    else do
                        isDir <- doesDirectoryExist path
                        if isDir
                            then do
                                entries <- listDirectory path
                                withCurrentDirectory path (foldMap go entries)
                            else pure []
            else pure []
    goFile path = case isHiePath path of
        Nothing -> []
        Just m -> [(m, path)]

lookupHiePath :: ModuleName -> HiePaths -> Maybe FilePath
lookupHiePath moduleName (HiePaths hiePaths) = Map.lookup moduleName hiePaths

data HieState = HieState
    { hiePaths :: HiePaths
    , nameCache :: NameCache
    , hieFiles :: MVar (Map ModuleName HieFile)
    }

lookupOrLoadHieFile :: HieState -> ModuleName -> IO (Maybe HieFile)
lookupOrLoadHieFile hieState moduleName = modifyMVar hieState.hieFiles \hieFiles -> do
    case Map.lookup moduleName hieFiles of
        Just hieFile -> pure (hieFiles, Just hieFile)
        Nothing -> case lookupHiePath moduleName hieState.hiePaths of
            Just hiePath -> do
                hieFile <- (.hie_file_result) <$> readHieFile hieState.nameCache hiePath
                pure (Map.insert moduleName hieFile hieFiles, Just hieFile)
            Nothing -> pure (hieFiles, Nothing)

newHieState :: [FilePath] -> IO HieState
newHieState paths = do
    nameCache <- initNameCache 'z' []
    hiePaths <- loadHiePaths paths
    hieFiles <- newMVar mempty
    pure $ HieState{nameCache, hiePaths, hieFiles}
