module Text.Bookbuilder -- TODO ( compile ) where
where

-- TODO: Test on only file
-- TODO: Test non-uniform extensions
-- TODO: Test '-' profile
-- TODO: Test on no src
-- TODO: Test broken templates
-- TODO: Test on multi/level/src
-- TODO: Test inputformats
-- TODO: add variable opts

import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( asks )
import Text.Bookbuilder.Config ( Configged, dest, profiles )
import Text.Bookbuilder.Profile ( Profile )
import qualified Text.Bookbuilder.Profile as Profile
import Text.Bookbuilder.Book ( Book, base, discover, populate, flatten )

write :: Profile -> String -> Configged IO ()
write prof content = do
	destination <- asks $ dest prof
	liftIO $ Profile.write prof destination content

output :: Book -> Profile -> Configged IO ()
output struct prof = write prof $ flatten prof struct

compile :: Configged IO ()
compile = do
	root <- liftIO =<< asks base
	filled <- discover root
	populated <- populate filled
	profs <- asks profiles
	mapM_ (output populated) profs
