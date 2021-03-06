module System.FilePath.Utils where

import Control.Applicative
import Control.Monad.Loops
import Data.List ( sort )
import System.Directory
import System.FilePath.Posix

from :: FilePath -> FilePath -> FilePath
from a b | makeRelative b a == "." = ""
         | otherwise = makeRelative b a

ls :: FilePath -> IO [FilePath]
ls path = (map (path </>) . sort . filter isVisible) <$>
	getDirectoryContents path

isVisible :: FilePath -> Bool
isVisible = not . (== '.') . head . takeFileName

exists :: FilePath -> IO Bool
exists p = (||) <$> doesDirectoryExist p <*> doesFileExist p

offerExtension :: String -> FilePath -> FilePath
offerExtension ext p | hasExtension p = p
                     | otherwise = p <.> ext

canonicalizeHere :: FilePath -> IO FilePath
canonicalizeHere = (canonicalizePath =<<) . unrel where
	unrel p | isRelative p = (</> p) <$> getCurrentDirectory
	        | otherwise = return p

ancestorWith :: FilePath -> [FilePath] -> IO (Maybe FilePath)
ancestorWith path children = checksOut >>= ancestorWith' where
	checksOut = allM (exists . (path </>)) children
	ancestorWith' True = return $ Just path
	ancestorWith' False | path /= next = ancestorWith next children
	ancestorWith' _ = return Nothing
	next = takeDirectory path
