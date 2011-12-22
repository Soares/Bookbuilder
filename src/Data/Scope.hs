module Data.Scope
	( Scope(Scope)
	, lower
	, upper
	, contains
	, fromTuple
	, toTuple
	, everywhere
	) where

import Control.Arrow ( (&&&) )
import Data.Focus ( Focus, toList, unfocused )
import Data.List ( intercalate )

data Scope = Scope Focus Focus deriving (Eq, Ord)

instance Show Scope where
	show (Scope lo hi) = "(" ++ (str lo) ++ "-" ++ (str hi) ++ ")" where
		str = intercalate "." . map show . toList

lower :: Scope -> Focus
lower (Scope lo _) = lo

upper :: Scope -> Focus
upper (Scope _ hi) = hi

contains :: Scope -> Focus -> Bool
contains (Scope lo hi) f = f >= lo && f <= hi

fromTuple :: (Focus, Focus) -> Scope
fromTuple (lo, hi) = Scope lo hi

toTuple :: Scope -> (Focus, Focus)
toTuple = lower &&& upper

everywhere :: Scope
everywhere = Scope unfocused unfocused
