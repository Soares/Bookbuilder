module Section.Variables ( variables ) where
import Data.Maybe ( fromMaybe )
import Data.Focus hiding ( focus )
import Data.Tree
import Data.Tree.Zipper
import Path ( title )
import Section.Isolate ( Isolate, name, isSameGender, focus )
import Section.Info

variables :: TreePos Full Isolate -> [(String, String)]
variables z = let
    -- Helper functions
    maybeLast [] = Nothing
    maybeLast (x:[]) = Just x
    maybeLast (_:xs) = maybeLast xs
    -- Data structure
    (Node isolate childs) = tree z
    loc = toList $ location z
    lefts = before z
    rights = after z
    smartLefts = takeWhile (isSameGender isolate . rootLabel) lefts
    smartRights = takeWhile (isSameGender isolate . rootLabel) rights
    ancestors = tail $ above z
    -- Simple variables
    n = fromMaybe 1 $ maybeLast loc
    counter = n - 1
    count = length lefts + length rights + 1
    smartCounter = length smartLefts
    smartN = smartCounter + 1
    smartCount = length smartLefts + length smartRights + 1
    childTitles = map (\(Node i _) -> (title . name) i) childs
    parentTitles = map (title . name) ancestors
    in [ ("title", title $ name isolate)
       , ("n", show n)
       , ("counter", show counter)
       , ("count", show count)
       , ("smartN", show smartN)
       , ("smartCounter", show smartCounter)
       , ("smartCount", show smartCount)
       , ("nChildren", show $ length childs) ] ++
       [ ("ns", show i) | i <- toList $ focus isolate ] ++
       [ ("child", c) | c <- childTitles ] ++
       [ ("parent" ++ show (i :: Int), p) |
         (i, p) <- zip [0..] parentTitles]
