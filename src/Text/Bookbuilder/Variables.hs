module Text.Bookbuilder.Variables ( variables ) where
import Data.Maybe ( fromMaybe )
import Data.Tree ( Tree(Node) )
import Data.Tree.Zipper ( tree, before, after )
import Text.Bookbuilder.Config ( Config )
import Text.Bookbuilder.Location ( toList )
import Text.Bookbuilder.Section
	( Structure
	, gender 
	, title
	, index
	, location
	, sections )

-- TODO: when location cahnges, add 'ns' list
variables :: Structure -> Config -> [(String, String)]
variables z conf = let
	-- Helper functions
	maybeLast [] = Nothing
	maybeLast (x:[]) = Just x
	maybeLast (_:xs) = maybeLast xs
	sameGender (Node s _) = gender s == gender section
	-- Data structure
	(Node section children) = tree z
	ss = sections z
	loc = toList $ location ss
	lefts = before z
	rights = after z
	smartLefts = takeWhile sameGender lefts
	smartRights = takeWhile sameGender rights
	ancestors = tail ss
	-- Simple variables
	n = fromMaybe 1 $ maybeLast loc
	counter = n - 1
	count = length lefts + length rights + 1
	smartCounter = length smartLefts
	smartN = smartCounter + 1
	smartCount = length smartLefts + length smartRights + 1
	childTitles = map (\(Node s _) -> title s conf) children
	parentTitles = map (flip title conf) ancestors
    in [ ("title", title section conf)
       , ("n", show n)
       , ("counter", show counter)
       , ("count", show count)
       , ("smartN", show smartN)
       , ("smartCounter", show smartCounter)
       , ("smartCount", show smartCount)
       , ("nChildren", show $ length children) ] ++
       [ ("ns", show i) | i <- toList $ index section ] ++
       [ ("child", c) | c <- childTitles ] ++
       [ ("parent" ++ show (i :: Int), p) |
         (i, p) <- zip [0..] parentTitles] where
