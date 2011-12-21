module Text.Bookbuilder.Template
	( Template
	, TemplateError
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

import Data.Functor ( (<$>) )
import Data.Function ( on )
import Data.Limit ( Limit(Bounded,Unbounded) )
import Data.Maybe ( fromMaybe )
import qualified Text.Bookbuilder.Template.Constraint as Constraint
import Text.Bookbuilder.Template.Constraint ( Constraint )
import Text.Bookbuilder.Location ( Location, toList )
import Text.Pandoc.Templates ( renderTemplate )
import Text.ParserCombinators.Parsec ( ParseError )
import Text.Printf ( printf )
import System.FilePath.Posix ( takeFileName, splitFileName )



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

load :: FilePath -> IO (Either TemplateError Template)
load path = load' $ Constraint.load file where
	file = takeFileName path
	load' (Left err) = return . Left $ NoParse path err
	load' (Right cs) = (Right . create cs) <$> readFile path
	create cs s = Template{ _name = file
	                      , _source = s
	                      , _constraints = cs }



-- | Constraint checking

matches :: Template -> Location -> Bool
matches tmpl = Constraint.matches (_constraints tmpl) . toList

matching :: [Template] -> Location -> Maybe Template
matching ts loc = first $ filter (`matches` loc) ts
	where first xs = if null xs then Nothing else Just $ head xs

get :: [Template] -> Location -> Template
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

data TemplateError = NoParse FilePath ParseError
instance Show TemplateError where
	show (NoParse f e) = let (template, profile) = splitFileName f in
		printf "WARNING: Could not parse template %s\n" template ++
		printf "Error was: %s\n" (show e) ++
		printf "In the profile: %s\n" profile
		"The error will not be used."
