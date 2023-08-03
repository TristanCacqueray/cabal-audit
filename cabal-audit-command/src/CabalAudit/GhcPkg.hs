-- | This module contains the logic to query ghc-pkg
module CabalAudit.GhcPkg where

import CabalAudit.LoadUtils (mkOsString)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Set (Set)
import Data.Set qualified as Set
import System.OsPath
import System.Process

-- | Returns every library-dirs registered in ghc-pkg database.
getLibraryDirs :: IO (Set OsString)
getLibraryDirs = do
    (_, Just hout, _, _) <-
        createProcess (ghcPkgCmd{std_out = CreatePipe})

    Set.fromList <$> readLibraryDirs hout False []
  where
    ghcPkgCmd = proc "ghc-pkg" ["--expand-pkgroot", "dump"]

    readLibraryDirs handle inLibDirs acc = do
        eLine <- try @SomeException $ BS.hGetLine handle
        case eLine of
            Left{} -> pure acc -- this is the end
            Right bline
                | inLibDirs -> case parsePath bline of
                    Just l -> readLibraryDirs handle True (l : acc)
                    Nothing -> readLibraryDirs handle False acc
                | otherwise -> readLibraryDirs handle (bline == "library-dirs:") acc

    parsePath :: BS.ByteString -> Maybe OsString
    parsePath bs
        | "    /" `BS.isPrefixOf` bs = Just (mkOsString $ SBS.toShort $ BS.dropWhile (== 0x20) bs)
        | otherwise = Nothing
