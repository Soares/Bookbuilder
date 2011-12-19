module Text.Bookbuilder.FilePath where
-- TODO: exports

import Control.Applicative ( (<*>) )
import Data.Functor ( (<$>) )
import Data.List ( sort )
import Data.List.Split ( split, startsWithOneOf )
import Data.List.Utils ( startswith )
import Data.String.Utils ( join )
import Text.Bookbuilder.Location ( Location(Location), separators )
import System.Directory
	( getCurrentDirectory
	, getDirectoryContents
	, canonicalizePath
	, doesDirectoryExist
	, doesFileExist )
import System.FilePath.Posix
	( (</>)
	, (<.>)
	, isRelative
	, makeRelative
	, dropExtension
	, hasExtension
	, splitPath
	, takeFileName
	, dropTrailingPathSeparator )
import System.Posix.Files ( fileSize, getFileStatus )

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

canonicalize :: FilePath -> IO FilePath
canonicalize = (canonicalizePath =<<) . unrel where
	unrel p | isRelative p = (</> p) <$> getCurrentDirectory
	        | otherwise = return p

from :: FilePath -> FilePath -> FilePath
from a b = if a == b then "" else makeRelative b a

pathLocation :: FilePath -> Location
pathLocation = Location . map index . splitPath where
	index = takeInt . takeFileName . dropTrailingPathSeparator
	takeInt = headOr 1 . map fst . reads
	headOr a xs = if null xs then a else head xs

pathTitle :: FilePath -> String
pathTitle = deCamel . dropExtension . dropIndex . filePart where
	filePart = takeFileName . dropTrailingPathSeparator
	dropable = separators ++ ['0'..'9']
	dropIndex = dropWhile (`elem` dropable)
	deCamel = join " " . split (startsWithOneOf ['A'..'Z'])
