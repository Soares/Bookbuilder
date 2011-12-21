module Text.Bookbuilder.FilePath where

import Control.Applicative ( (<*>) )
import Control.Monad.Loops ( allM )
import Data.Functor ( (<$>) )
import Data.List ( sort )
import Data.List.Utils ( startswith )
import System.Directory
	( canonicalizePath
	, doesDirectoryExist
	, doesFileExist
	, getCurrentDirectory
	, getDirectoryContents )
import System.FilePath.Posix
	( (</>)
	, (<.>)
	, isRelative
	, makeRelative
	, hasExtension
	, takeDirectory
	, takeFileName )
import System.Posix.Files ( fileSize, getFileStatus )

from :: FilePath -> FilePath -> FilePath
from a b | b `startswith` a = ""
         | otherwise = makeRelative b a

ls :: FilePath -> IO [FilePath]
ls path = (map (path </>) . sort . filter isVisible) <$>
	getDirectoryContents path

isVisible :: FilePath -> Bool
isVisible = not . startswith "." . takeFileName

exists :: FilePath -> IO Bool
exists p = (||) <$> doesDirectoryExist p <*> doesFileExist p

fileHasData :: FilePath -> IO Bool
fileHasData file = fmap ((> 0) . fileSize) (getFileStatus file)

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
