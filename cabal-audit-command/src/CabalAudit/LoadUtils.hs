module CabalAudit.LoadUtils where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Char (isUpper)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Data.FastString (FastString, mkFastStringShortByteString)
import GHC.Unit.Module (ModuleName, mkModuleNameFS)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OSP
import System.OsString.Internal.Types
import System.Process

findModulesPath :: OsPath -> [OsPath] -> IO (Map ModuleName OsPath)
findModulesPath desiredExt rootPaths = do
    paths <- getPaths desiredExt rootPaths
    pure . Map.fromList . mapMaybe getPath $ paths
  where
    getPath :: OsPath -> Maybe (ModuleName, OsPath)
    getPath fp = case isModulePath desiredExt fp of
        Nothing -> Nothing
        Just moduleName -> Just (moduleName, fp)

{- | Get module name from a path

 >>> isModulePath [osp|.hie|] [osp|./dist-newstyle/build/Riri/Fifi/Loulou.hie|]
 Just (ModuleName "Riri.Fifi.Loulou")
-}
isModulePath :: OsPath -> OsPath -> Maybe ModuleName
isModulePath desiredExt filePath = case OSP.splitExtensions filePath of
    (fp, ext)
        | ext == desiredExt -> Just $ mkModuleNameFS (toModuleName fp)
        | otherwise -> Nothing
  where
    toModuleName :: OsPath -> FastString
    toModuleName =
        mkFastStringShortByteString
            . SBS.intercalate "."
            . map (coerce :: OsPath -> ShortByteString)
            . reverse
            . takeWhile isModule
            . reverse
            . OSP.splitDirectories
    isModule :: OsPath -> Bool
    isModule path = case OSP.unpack path of
        (c : _) | isUpper (OSP.toChar c) -> True
        _ -> False

getPaths :: OsPath -> [OsPath] -> IO [OsPath]
getPaths desiredExt rootPaths = do
    findCmd <- mkOsCommand [osp|find|] (rootPaths <> [[osp|-type|], [osp|f|], [osp|-name|], [osp|*|] <> desiredExt])
    (_, Just hout, _, _) <-
        createProcess (findCmd{std_out = CreatePipe})
    readOsPath hout []
  where
    readOsPath handle acc = do
        eLine <- try @SomeException $ BS.hGetLine handle
        case eLine of
            Left{} -> pure acc
            Right bline -> readOsPath handle (mkOsString (SBS.toShort bline) : acc)

mkOsCommand :: OsString -> [OsString] -> IO _
mkOsCommand cmd args = do
    cmdStr <- OSP.decodeFS cmd
    argsStr <- traverse OSP.decodeFS args
    pure $ proc cmdStr argsStr

{- | Convert a path from the output of a tool like find or ghc-pkg into a OsString
 TODO: handle non-posix strings?
-}
mkOsString :: SBS.ShortByteString -> OsString
mkOsString = OsString . PosixString
