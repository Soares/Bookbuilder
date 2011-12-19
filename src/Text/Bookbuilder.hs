module Text.Bookbuilder ( compile ) where

-- TODO: Test on only file
-- TODO: Test on no src
-- TODO: Test on multi/level/src
-- TODO: Allow output to places other than .tex
-- TODO: add writer variables
--     parent, book, count, total, N, parentN, countN, totalN, fontsize, author, etc.

import Control.Applicative ( (<*>) )
import Control.Monad ( filterM )
import Control.Monad.Loops ( andM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( asks )
import Data.Char ( toLower )
import Data.Functor ( (<$>) )
import Data.Maybe ( fromJust )
import Data.Tree ( Tree(Node) )
import System.Directory ( doesFileExist )
import System.FilePath.Posix ( takeExtension )
import Text.Bookbuilder.Config
	( Configged
	, srcDir
	, range
	, dest
	, operativePart
	, template
	, location )
import Text.Bookbuilder.FilePath ( ls, pathTitle, pathLocation, fileHasData )
import Text.Bookbuilder.Location ( toList )
import Text.Bookbuilder.Template ( Template, source )
import Text.Pandoc
	( Pandoc(Pandoc)
	, Meta(Meta)
	, Inline(Str)
	, readers
	, writeLaTeX
	, defaultParserState
	, defaultWriterOptions
	, WriterOptions(..) )
import Text.Pandoc.Templates ( renderTemplate )





-- | Prelude utilities              ====================================

contains :: (Ord a) => (a, a) -> a -> Bool
contains (lo, hi) x = x >= lo && x <= hi



-- | Control.Monad utilities        ====================================

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= (\r -> if r then x else y)



-- | Text.Pandoc utilities          ====================================

pandocName :: FilePath -> String
pandocName p = case takeExtension (map toLower p) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".lhs"      -> "markdown+lhs"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
    _           -> "markdown"

withDefaultTitle :: Pandoc -> String -> Pandoc
withDefaultTitle (Pandoc (Meta [] as d) bs) t = Pandoc (Meta [Str t] as d) bs
withDefaultTitle doc _ = doc



-- | Bookbuilder tree manipulation  ====================================

buildTree :: FilePath -> IO (Tree FilePath)
buildTree path = ifM (doesFileExist path)
	(return $ Node path [])
	(Node path <$> (mapM buildTree =<< ls path))

prune :: Tree FilePath -> Configged IO (Tree FilePath)
prune (Node p cs) = Node p <$> (filterM keep =<< mapM prune cs)

keep :: Tree FilePath -> Configged IO Bool
keep node = (&&) <$> liftIO (hasContent node) <*> isInRange node

hasContent :: Tree FilePath -> IO Bool
hasContent (Node p []) = andM [doesFileExist p, fileHasData p]
hasContent _ = return True

isInRange :: Tree FilePath -> Configged IO Bool
isInRange (Node t _) = contains <$> asks range <*> asks (location t)



-- | File and template rendering    ====================================

render :: String -> String -> String
render base contents = write $ parse contents `withDefaultTitle` title where
	write = writeLaTeX defaultWriterOptions{ writerChapters = True }
	parse = (fromJust $ lookup pname readers) defaultParserState
	pname = pandocName base
	title = pathTitle base

wrap :: [(String, String)] -> Template -> String
wrap vars tmpl = renderTemplate vars $ source tmpl

variables :: FilePath -> String -> Configged IO [(String, String)]
variables path body = do
	base <- asks $ operativePart path
	return [ ("body", body)
	       , ("title", pathTitle base)
		   , ("n", maybe "" show n)
		   , ("n0", maybe "" show n0)
		   -- TODO: nLessN should be obsoleted when we use zippers
		   , ("nLess2", maybe "" show nLess2)
		   , ("nLess3", maybe "" show nLess3)
		   ] where
	loc = toList $ pathLocation path
	n = maybeLast loc
	n0 = (flip (-) 1) <$> n
	nLess2 = (flip (-) 2) <$> n
	nLess3 = (flip (-) 3) <$> n
	maybeLast [] = Nothing
	maybeLast (x:[]) = Just x
	maybeLast (_:xs) = maybeLast xs



-- | Bookbuilder compilation        ====================================

flatten :: Tree FilePath -> Configged IO String
flatten (Node path children) = do
	leaf <- liftIO $ doesFileExist path
	tmpl <- asks $ template path
	body <- if leaf
		then render <$> asks (operativePart path) <*> liftIO (readFile path)
		else concat <$> mapM flatten children
	vars <- variables path body
	return $ wrap vars tmpl

compile :: Configged IO ()
compile = do
	tree <- liftIO =<< buildTree <$> asks srcDir
	content <- flatten =<< prune tree
	destination <- asks dest
	let write = maybe putStr writeFile destination
	liftIO $ write content
