module Text.Bookbuilder.Variables ( variables ) where
import Data.Maybe ( fromMaybe )
import Data.Focus
import Data.Tree ( Tree(Node) )
import Data.Tree.Zipper ( tree, before, after )
import qualified Path
import qualified Section.Isolate as Isolate
import Section.Info

variables :: Section -> [(String, String)]
variables z conf = let
	-- Helper functions
	maybeLast [] = Nothing
	maybeLast (x:[]) = Just x
	maybeLast (_:xs) = maybeLast xs
	sameGender (Node i _) = gender i == gender isolate
	-- Data structure
	(Node isolate children) = tree z
    as = above z
	loc = toList $ location z
	lefts = before z
	rights = after z
	smartLefts = takeWhile sameGender lefts
	smartRights = takeWhile sameGender rights
	ancestors = tail as
	-- Simple variables
	n = fromMaybe 1 $ maybeLast loc
	counter = n - 1
	count = length lefts + length rights + 1
	smartCounter = length smartLefts
	smartN = smartCounter + 1
	smartCount = length smartLefts + length smartRights + 1
	childTitles = map (\(Node i _) -> title i conf) children
	parentTitles = map (`title` conf) ancestors
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
         (i, p) <- zip [0..] parentTitles]
