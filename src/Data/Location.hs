module Data.Location
	( Location
	, Range
	, toList
	, fromList
	, fromString
	, stripLocation
	, stripLocation'
	, splitString
	, focus
	, contract
	, retract
	, nowhere
	) where

import Data.List.Split ( splitOn )
import Data.List.Utils ( join )
import Data.String.Utils ( lstrip )

data Location = Location { _list :: [Int] }
type Range = (Location, Location)

-- NOTE: If one location is the beginning of another, they are 'equal'.
-- Think of further depth in the Location as refinement.
-- For real tests of equality, check the underlying list.
instance Eq Location where
	xs == ys = common xs ys == common ys xs
instance Ord Location where
	xs <= ys = common xs ys <= common ys xs



-- | Exported behavior

toList :: Location -> [Int]
toList = _list

fromList :: [Int] -> Location
fromList = Location

splitString :: String -> (Maybe Location, String)
splitString str = splitString' $ reads str where
	splitString' [] = (Nothing, str)
	splitString' ((loc, rest):_) = (Just loc, rest)

fromString :: String -> Maybe Location
fromString = fst . splitString

stripLocation :: String -> String
stripLocation = snd . splitString

stripLocation' :: String -> String
stripLocation' = dropWhile (`elem` separators) . stripLocation

focus :: Location -> Location -> Location
focus (Location xs) (Location ys) = Location $ xs ++ ys

contract :: Location -> Int -> Location
contract (Location xs) x = Location $ xs ++ [x]

retract :: Location -> Location
retract (Location []) = Location []
retract (Location xs) = Location $ init xs

nowhere :: Location
nowhere = Location []

separators :: String
separators = " .,;:|-_"



-- | Helpers

common :: Location -> Location -> [Int]
common xs ys = take enough $ toList xs
	where enough = min (length $ toList xs) (length $ toList ys)



-- | Reading and Showing

instance Show Location where
	show (Location xs) = "<" ++ join "|" (map show xs) ++ ">"

-- NOTE: Location reads *very* promiscuously, so that it may read from a
-- variety of numbering formats on user-created file names. Watch out!
-- it might eat more text than you want.
instance Read Location where
	readsPrec = const readLocation

-- Locations can be in angle brackets or may be unadorned
readLocation :: ReadS Location
readLocation s = [(Location xs, rest) | ("<", t) <- lex s
                                      , (xs, '>':rest) <- readInts t]
                 ++ [(Location xs, rest) | (xs, rest) <- readInts s]

readInts :: ReadS [Int]
readInts s = [(x : xs, rest) | (x, t) <- readInt s
                             , (_, u) <- readSep t
                             , (xs, rest) <- readInts u]
             ++ [(x : xs, rest) | (x, t) <- readInt s
			                    , (xs, rest) <- readInts t]
             ++ [([x], rest) | (x, rest) <- readInt s]

-- This is a bit hackish, but we can't use the default implementation of
-- reads :: ReadS Int because it uses 'lex' instead of 'dotlex'
readInt :: ReadS Int
readInt s = [(x, rest++leftover) | (str, leftover) <- dotlex s
                                 , (x, rest) <- reads str]

readSep :: ReadS String
readSep s = [(sep, rest) | (sep, rest) <- dotlex s
                         , sep `elem` map return separators]

-- A version of 'lex' that splits on dots as well, allowing us to parse
-- something like "1.2.3" as multiple numbers
dotlex :: ReadS String
dotlex = dotlex' . lstrip where
	dotlex' ('.':s) = [(".",s)]
	dotlex' s = [(a, b++rest) | (tok, rest) <- lex s
	                          , let (a:bs) = splitOn "." tok
	                          , let b = join "." ("":bs)]
