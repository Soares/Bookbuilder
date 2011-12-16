module Text.Bookbuilder.Template
	( Template(name,theme,source,extension)
	, matches
	, fromFile
	, fallback
	) where

import Data.Bound ( Bound(Unbounded) )
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
	, constraints  :: Bound [Constraint]
	} deriving Show

instance Eq Template where (==) = (==) `on` name
instance Ord Template where (<=) = (<=) `on` constraints

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
		shortCircuit err = print err >> return Nothing
		create contents = Template { name = takeFileName path
		                           , source = contents
		                           , theme = dropExtension leftover
		                           , extension = takeExtension leftover
		                           , constraints = cs }
