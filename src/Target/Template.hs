module Target.Template
	( Template
	, Error
	, render
	, name
	, load
	, matches
	, matching
	, get
	, isTop
	, isAny
	, fallback
	) where

import Data.Focus ( Focus, toList )
import Data.Functor ( (<$>) )
import Data.Function ( on )
import Data.Limit ( Limit(Bounded,Unbounded) )
import Data.Maybe ( fromMaybe )
import Target.Constraint ( Constraint )
import qualified Target.Constraint as Constraint
import Text.Pandoc.Templates ( renderTemplate )
import Text.ParserCombinators.Parsec ( ParseError )
import Text.Printf ( printf )
import System.FilePath ( takeFileName, splitFileName )



data Template = Template
	{ _name         :: String
	, _source       :: String
	, _constraints  :: Limit [Constraint] }

instance Eq Template where (==) = (==) `on` _name
instance Ord Template where (<=) = (<=) `on` _constraints
instance Show Template where
	show t = name t ++ cs where
		cs = case _constraints t of
			Unbounded -> "@"
			Bounded xs -> show xs



-- | Accessors

name :: Template -> String
name = _name

render :: [(String, String)] -> Template -> String
render vars = renderTemplate vars . _source



-- | Creation

load :: FilePath -> IO (Either Error Template)
load path = load' $ Constraint.load file where
	file = takeFileName path
	load' (Left err) = return . Left $ NoParse path err
	load' (Right cs) = (Right . create cs) <$> readFile path
	create cs s = Template{ _name = file
	                      , _source = s
	                      , _constraints = cs }



-- | Constraint checking

matches :: Template -> Focus -> Bool
matches tmpl = Constraint.matches (_constraints tmpl) . toList

matching :: [Template] -> Focus -> Maybe Template
matching ts loc = first $ filter (`matches` loc) ts
	where first xs = if null xs then Nothing else Just $ head xs

get :: [Template] -> Focus -> Template
get ts = fromMaybe fallback . matching ts



-- | Special templates

isTop :: Template -> Bool
isTop t = _constraints t == Bounded []

isAny :: Template -> Bool
isAny t = _constraints t == Unbounded

fallback :: Template
fallback = Template { _name = "fallback"
					, _source = "# $title$ #\n$body$\n"
					, _constraints = Unbounded }



-- | Error handling

data Error = NoParse FilePath ParseError
instance Show Error where
	show (NoParse f e) = let (template, profile) = splitFileName f in
		printf "WARNING: Could not parse template %s\n" template ++
		printf "\tError was: %s\n" (show e) ++
		printf "\tIn the profile: %s\n" profile
		"\tThe template will not be used."
