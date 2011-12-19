module Text.Bookbuilder -- ( compile ) where
where

-- TODO: Test on only file
-- TODO: Test on no src
-- TODO: Test on multi/level/src
-- TODO: support more writers than writeLaTeX
-- TODO: add some pandoc writer variables (fontsize, author, etc.)
-- TODO: Consider the Config order of most accesors. (Reader anyone?)

import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( asks )
import Data.Char ( toLower )
import Data.Functor ( (<$>) )
import Data.Maybe ( fromMaybe, fromJust, catMaybes )
import Data.Tree ( Tree(Node) )
import qualified Data.Tree.Zipper as Zipper
import System.Directory ( doesFileExist )
import System.FilePath.Posix ( takeExtension )
import Text.Bookbuilder.Config
	( Config
	, Configged
	, range
	, dest
	, childrenOf
	, fullPath
	, title
	, template
	, location )
import Text.Bookbuilder.Location ( toList )
import Text.Bookbuilder.Template ( Template, source )
import Text.Pandoc
	( readers
	, writeLaTeX
	, defaultParserState
	, defaultWriterOptions
	, WriterOptions(..) )
import Text.Pandoc.Templates ( renderTemplate )





-- | Prelude utilities              ====================================

contains :: (Ord a) => (a, a) -> a -> Bool
contains (lo, hi) x = x >= lo && x <= hi



-- | Text.Pandoc utilities          ====================================

format :: FilePath -> String
format p = case takeExtension (map toLower p) of
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



-- | Bookbuilder tree manipulation  ====================================

data Gender = File | Directory deriving (Eq, Show, Read)
type Section = (FilePath, Gender)
type Structure = Tree Section
type Position = Zipper.TreePos Zipper.Full Section

buildTree :: String -> Configged IO Structure
buildTree path = do
	file <- asks $ fullPath path
	isFile <- liftIO $ doesFileExist file
	let node = if isFile then fileNode else dirNode
	node path

fileNode :: String -> Configged IO Structure
fileNode path = return $ Node (path, File) []

dirNode :: String -> Configged IO Structure
dirNode path = do
	childPaths <- liftIO =<< asks (childrenOf path)
	children <- mapM buildTree childPaths
	return $ Node (path, Directory) children

prune :: Structure -> Config -> Structure
prune (Node s cs) conf = Node s $ filter (`isInRange` conf) pruned
	where pruned = map (`prune` conf) cs

isInRange :: Structure -> Config -> Bool
isInRange (Node (p, _) _) conf = contains (range conf) (location p)

childrenPositions :: Position -> [Int] -> [Position]
childrenPositions z is = catMaybes [Zipper.childAt i z | i <- is]



-- | File and template rendering    ====================================

render :: String -> String -> String
render fmt text = write $ parse text where
	write = writeLaTeX defaultWriterOptions{ writerChapters = True }
	parse = (fromJust $ lookup fmt readers) defaultParserState

wrap :: [(String, String)] -> Template -> String
wrap vars tmpl = renderTemplate vars $ source tmpl

simplevars :: Position -> Config -> [(String, String)]
simplevars z conf = [ ("title", title path conf)
                    , ("n", show n)
                    , ("counter", show counter)
                    , ("count", show count)
                    , ("smartN", show smartN)
                    , ("smartCounter", show smartCounter)
                    , ("smartCount", show smartCount)
                    , ("nChildren", show $ length children) ] ++
                    [ ("child", c) | c <- childTitles ] ++
                    [ ("parent" ++ show (i :: Int), p) |
                      (i, p) <- zip [0..] parentTitles] where
	-- Helper functions
	maybeLast [] = Nothing
	maybeLast (x:[]) = Just x
	maybeLast (_:xs) = maybeLast xs
	sameGender (Node (_, g) _) = gender == g
	-- Data structure
	(Node (path, gender) children) = Zipper.tree z
	loc = toList $ location path
	lefts = Zipper.before z
	rights = Zipper.after z
	smartLefts = takeWhile sameGender lefts
	smartRights = takeWhile sameGender rights
	parents = map (\(_, p, _) -> p) $ Zipper.parents z
	-- Simple variables
	n = fromMaybe 1 $ maybeLast loc
	counter = n - 1
	count = length lefts + length rights + 1
	smartCounter = length smartLefts
	smartN = smartCounter + 1
	smartCount = length smartLefts + length smartRights + 1
	childTitles = map (\(Node (p, _) _) -> title p conf) children
	parentTitles = map (\(p, _) -> title p conf) parents


variables :: Position -> String -> Config -> [(String, String)]
variables z body conf = ("body", body) : simplevars z conf



-- | Bookbuilder compilation        ====================================

flatten :: Position -> Configged IO String
flatten z = do
	let (Node (path, gender) cs) = Zipper.tree z
	body <- case gender of
		File -> do
			file <- asks $ fullPath path
			content <- liftIO $ readFile file
			return $ render (format path) content
		Directory -> do
			let children = childrenPositions z [0..length cs - 1]
			concat <$> mapM flatten children
	vars <- asks $ variables z body
	tmpl <- asks $ template path
	return $ wrap vars tmpl

compile :: Configged IO ()
compile = do
	tree <- buildTree ""
	pruned <- asks $ prune tree
	content <- flatten $ Zipper.fromTree pruned
	destination <- asks dest
	let write = maybe putStr writeFile destination
	liftIO $ write content
