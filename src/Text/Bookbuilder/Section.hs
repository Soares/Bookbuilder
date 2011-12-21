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
	, fill
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
	, takeFileName
	, splitFileName )
import Text.Bookbuilder.Config ( Config, root, src, isInRange, context )
import Text.Bookbuilder.FilePath ( ls )
import Text.Bookbuilder.Location
	( Location
	, fromString
	, stripLocation'
	, focus
	, nowhere )
import Text.Bookbuilder.Profile ( Profile, template, lift )
import Text.Bookbuilder.Template ( render )

data Gender = File | Directory deriving (Eq, Show, Read)

data Section = Section { _path    :: String
                       , _context :: [(String, String)]
					   , _body    :: String
					   , _gender  :: Gender
                       } deriving (Eq, Show, Read)

type Structure = TreePos Full Section

section :: (FilePath, String) -> [(String, String)] -> IO Section
section (dir, name) vars = let path = dir </> name in do
	isDir <- doesDirectoryExist path
	let g = if isDir then Directory else File
	return Section{ _path = name, _context = vars, _gender = g, _body = "" }

base :: Config -> IO Section
base conf = section (src conf, "") (context conf)

gender :: Section -> Gender
gender = _gender

title :: Section -> Config -> String
title s c | _path s == "" = title' $ takeFileName $ root c
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
reduce prof ss@(s:_) = render vars (template prof $ location ss) where
	vars = ("body", text $ gender s) : _context s
	text File = lift prof (_path s) (_body s)
	text Directory = _body s
reduce _ _ = ""

subsections :: [Section] -> Config -> IO [Section]
subsections [] conf = return <$> base conf
subsections (s:_) _ | gender s == File = return []
subsections ss conf = ls (filepath ss conf) >>= mapM create where
	create p = section (splitFileName p) (context conf)

contextualize :: Section -> [(String, String)] -> Section
contextualize s vars = s{ _context = vars ++ (_context s) }

fill :: Section -> String -> Section
fill s body = s{ _body = body }

decide :: String -> String -> Section -> Section
decide k v s = s{ _context = (k, v) : _context s }
