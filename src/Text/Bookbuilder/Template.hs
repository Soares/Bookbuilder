module Text.Bookbuilder.Template
	( Template(name,theme,source,extension)
	, matches
	, fromFile
	, fallback
	) where

import Control.Monad ( liftM, liftM2, foldM )
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

fallback :: String -> Template
fallback theme = Template { name = "fallback"
                          , theme = theme
						  , source = "$body$\n"
						  , extension = ".tex"
						  , constraints = Unbounded }

matches :: Template -> [Integer] -> Bool
matches = check . constraints

fromFile :: FilePath -> IO (Maybe Template)
fromFile path = handle $ fromName $ takeFileName path where
	handle (Left error) = print error >> return Nothing
	handle (Right (cs, tail)) = catch attempt fail where
		attempt = fmap (Just . create) (readFile path)
		fail err = print err >> return Nothing
		create source = Template { name = takeFileName path
		                         , source = source
		                         , theme = dropExtension tail
		                         , extension = takeExtension tail
		                         , constraints = cs }
