module Text.Bookbuilder.Section
	( Section
	, Gender(..)
	, Structure
	, base
	, title
	, index
	, gender
	, above
	, valid
	, sections
	, filepath
	, location
	, subsections
	, contextualize
	, decide
	, reduce
	) where

import Data.Functor ( (<$>) )
import Data.List.Utils ( join )
import Data.List.Split ( split, startsWithOneOf )
import Data.Maybe ( fromMaybe )
import Data.Tree.Zipper ( TreePos, PosType, Full, label, parents )
import System.Directory ( doesDirectoryExist )
import System.FilePath.Posix
	( (</>)
	, joinPath
	, dropExtension
	, takeFileName )
import Text.Bookbuilder.Config ( Config, root, src, isInRange, context )
import Text.Bookbuilder.FilePath ( ls )
import Text.Bookbuilder.Location
	( Location
	, fromString
	, stripLocation'
	, focus
	, nowhere )
import Text.Bookbuilder.Profile ( Profile, template )
import Text.Bookbuilder.Template ( render )

data Gender = File | Directory deriving (Eq, Show, Read)

data Section = Section { _path    :: String
                       , _context :: [(String, String)]
					   , _gender  :: Gender
                       } deriving (Eq, Show, Read)

type Structure = TreePos Full Section

section :: FilePath -> [(String, String)] -> IO Section
section path vars = do
	isDir <- doesDirectoryExist path
	let g = if isDir then Directory else File
	let name = takeFileName path
	return Section{ _path = name, _context = vars, _gender = g }

base :: Config -> IO Section
base conf = section (root conf) (context conf)

gender :: Section -> Gender
gender = _gender

title :: Section -> Config -> String
title s c | _path s == "" = title' $ root c
          | otherwise = title' $ _path s where
	title' = deCamel . dropExtension . stripLocation'
	deCamel = join " " . split (startsWithOneOf ['A'..'Z'])

index :: Section -> Location
index = fromMaybe nowhere . fromString . _path

above :: PosType t => TreePos t Section -> [Section]
above = map (\(_, s, _) -> s) . parents

sections :: TreePos Full Section -> [Section]
sections z = label z : above z

valid :: [Section] -> Config -> Bool
valid ss conf = isInRange (location ss) conf

filepath :: [Section] -> Config -> FilePath
filepath ss conf = src conf </> (joinPath $ reverse $ map _path ss)

location :: [Section] -> Location
location = foldr (focus . index) nowhere . reverse

reduce :: Profile -> [Section] -> String
reduce prof (s:ss) = render (_context s) (template prof $ location $ s:ss)
reduce _ _ = ""

subsections :: [Section] -> Config -> IO [Section]
subsections [] conf = return <$> base conf
subsections (s:_) _ | gender s == File = return []
subsections ss conf = ls path >>= mapM create where
	create p = section (path </> p) (context conf)
	path = filepath ss conf

contextualize :: Section -> [(String, String)] -> Section
contextualize s vars = s{ _context = vars }

decide :: String -> String -> Section -> Section
decide k v s = s{ _context = (k, v) : _context s }
