module Text.Bookbuilder ( Target, targets, output ) where

-- TODO:
--		rename the profiles dir to targets
--		add a 'targets' option
--			to limit the targets used
--		add context file support
--			profiles/context and profiles/*/context
--			remove 'author' after having done so
--		add config file support
--			profiles/config and profiles/*/config
--			add support as writer options
--		change Config to Environment
--			expose targets instead of profiles
--			profiles named 'default' should output with the title name
--			split 'Options' out of 'Environment'
--		make a Book/ namespace
--			turn all the functions of [Section] into functions of Position
--			make sure that environment starts with the base position
--		add a 'silent' option
--		add a flag to signify "this is src, everything else is one level up"
--
-- CHECK:
--		whether or not we need newlines at the end of templates
--			case in point: remove newlines from default.epub md files
-- Test:
--		on a book where src/ is a file
--		on a book where src is given as multiple/level/src/
--		on a book with internally different input formats
--		on a book where src/ is above the book

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ask, asks )
import System.FilePath.Posix ( (<.>) )
import Text.Bookbuilder.Config ( Configged, dest, profiles )
import Text.Bookbuilder.Profile ( Profile, write, buildFormat )
import Text.Bookbuilder.Book ( Book, base, discover, populate, flatten )

data Target = Target { _destination :: FilePath
                     , _profile     :: Profile
                     , _contents    :: Book }

instance Show Target where
	show t = (_destination t) ++ " [" ++ show (_profile t) ++ "]"

output :: Bool -> Target -> IO ()
output debug (Target path prof book) = do
	when debug $ writeFile (path <.> buildFormat prof) text
	write prof path text
	where text = flatten prof book

targets :: Configged IO [Target]
targets = do
	root <- liftIO =<< asks base
	filled <- discover root
	book <- populate filled
	profs <- asks profiles
	config <- ask
	return [Target (dest prof config) prof book | prof <- profs]
