module Text.Bookbuilder -- TODO ( compile ) where
where

-- TODO: default profile -> title name
-- TODO: add config file (top level & profile specific)
--		author
--		pdf file
-- TODO: Test on only file
-- TODO: Test non-uniform extensions
-- TODO: Test '-' profile
-- TODO: Test on no src
-- TODO: Test broken templates
-- TODO: Test on multi/level/src
-- TODO: Test inputformats
-- TODO: Suppress PDF's output of progress
-- TODO: add our own output of progress
-- TODO: add variable opts (or context files)
-- TODO: we're eating a lot of template lines (and putting nbsps in them.)
--		obviously we're misusing pandoc somehow
-- TODO: add debug opt
-- TODO: add silent option
-- TODO: add 'cautious' option
-- TODO: add 'profiles' limiting option
-- TODO: print status updates
-- TODO: hlint

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ask, asks )
import System.FilePath.Posix ( (</>), (<.>), takeDirectory )
import Text.Bookbuilder.Config ( Configged, dest, profiles )
import Text.Bookbuilder.Profile ( Profile, write, buildFormat )
import Text.Bookbuilder.Book ( Book, base, discover, populate, flatten )

data Target = Target { _destination :: FilePath
                     , _profile     :: Profile
                     , _contents    :: Book }

instance Show Target where
	show t = takeDirectory (_destination t) </> show (_profile t)

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
