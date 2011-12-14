module Build where

import Control.Monad.Reader
import Text.Pandoc

import Config

root = "/home/nate/Dropbox/Projects/Books/Resurgence"

type Name = String
type Title = String
data DocType = Novel
             | Strand Name
						 | Chapter Title

strandsIn :: Config -> IO [String]
strandsIn = return $ return $ ["one", "two"]

combineStrands :: [Pandoc] -> Pandoc
combineStrands (p:ps) = p

destination :: Config -> String
destination conf = "nowhere"

main :: IO ()
main = runReaderT (compile Novel) (Config root)

compile :: DocType -> ConfigedIO ()
compile Novel = do
	iostrands <- asks strandsIn

	strandNames <- lift iostrands

	strands <- mapM compile $ (map Strand) strandNames

	doc <- lift combineStrands strands

	dest <- asks destination Novel
	writeFile dest $ writeDoc EPUB doc
