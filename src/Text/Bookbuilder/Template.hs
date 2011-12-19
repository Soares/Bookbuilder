module Text.Bookbuilder.Template
	( Template(name,theme,source,extension)
	, matches
	, fromFile
	, fallback
	, isDefault
	, defaultTheme
	) where

import Prelude hiding ( catch )
import Control.Arrow ( (&&&) )
import Control.Exception ( catch )
import Data.Limit ( Limit(Unbounded) )
import Data.Function ( on )
import Text.Bookbuilder.Template.Constraint ( Constraint, check, fromName )
import System.FilePath.Posix
	( dropExtension
	, takeExtension
	, takeFileName )

data Template = Template
	{ name         :: String
	, theme        :: String
	, source       :: String
	, extension    :: String
	, constraints  :: Limit [Constraint]
	} deriving Show

defaultTheme :: String
defaultTheme = "default"

isDefault :: Template -> Bool
isDefault t = theme t == defaultTheme

instance Eq Template where (==) = (==) `on` name
instance Ord Template where (<=) = (<=) `on` (isDefault &&& constraints)

fallback :: Template
fallback = Template { name = "fallback"
                    , theme = "fallback"
					, source = "$body$\n"
					, extension = ".tex"
					, constraints = Unbounded }

matches :: Template -> [Integer] -> Bool
matches = check . constraints

fromFile :: FilePath -> IO (Maybe Template)
fromFile path = handle $ fromName $ takeFileName path where
	handle (Left err) = print err >> return Nothing
	handle (Right (cs, leftover)) = catch attempt shortCircuit where
		attempt = fmap (Just . create) (readFile path)
		shortCircuit :: IOError -> IO (Maybe Template)
		shortCircuit err = print err >> return Nothing
		create contents = Template { name = takeFileName path
		                           , source = contents
		                           , theme = dropExtension leftover
		                           , extension = takeExtension leftover
		                           , constraints = cs }
