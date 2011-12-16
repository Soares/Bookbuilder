module Text.Bookbuilder.Location
	( Location(Location,list)
	, fromName
	, empty
	) where

import Control.Monad ( liftM, liftM2, foldM )
import Data.String.Utils ( join )
import Text.Regex.Posix ( (=~) )

data Location = Location { list :: [Integer] }

empty :: Location
empty = Location []

-- NOTE: If one location is the beginning of another, they are 'equal'.
-- Think of further depth in the Location as refinement.
-- For real tests of equality, check the underlying list.
instance Eq Location where
	xs == ys = (common xs ys) == (common ys xs)
instance Ord Location where
	xs <= ys = (common xs ys) == (common ys xs)
instance Show Location where
	show (Location xs) = "<" ++ join "|" (map show xs) ++ ">"

common :: Location -> Location -> [Integer]
common xs ys = take enough $ list xs
	where enough = min (length $ list xs) (length $ list ys)

fromName :: String -> Integer
fromName name = if null nums then 1 else read nums
	where nums = name =~ "^([0-9]+)"
