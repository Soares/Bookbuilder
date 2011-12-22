module Data.Focus
    ( Focus
    , toList
    , fromList
    , fromString
    , parse
    , split
    , strip
    , focus
    , contract
    , retract
    , unfocused
    ) where

import Control.Arrow ( first )
import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import Data.String.Utils ( lstrip )

data Focus = Focus { _list :: [Int] }
instance Eq Focus where x == y = common x y == common y x
instance Ord Focus where xs <= ys = common xs ys <= common ys xs

common :: Focus -> Focus -> [Int]
common xs ys = take enough $ toList xs
    where enough = min (length $ toList xs) (length $ toList ys)


-- | Reading and Showing

instance Show Focus where
    show (Focus xs) = "<" ++ intercalate "|" (map show xs) ++ ">"

instance Read Focus where
    readsPrec _ s = [(Focus xs, rest) | ("<", s')      <- lex s
                                      , (xs, '>':rest) <- readInts s']
        where readInts t = [(x : xs, rest) | (x, t')    <- reads t
                                           , ("|", t'') <- lex t'
                                           , (xs, rest) <- readInts t'']
                           ++ map (first return) (reads t)


-- | Behavior

focus :: Focus -> Focus -> Focus
focus (Focus xs) (Focus ys) = Focus $ xs ++ ys

contract :: Focus -> Int -> Focus
contract (Focus xs) x = Focus $ xs ++ [x]

retract :: Focus -> Focus
retract (Focus []) = unfocused
retract (Focus xs) = Focus $ init xs

unfocused :: Focus
unfocused = fromList []


-- | Creation

toList :: Focus -> [Int]
toList = _list

fromList :: [Int] -> Focus
fromList = Focus

fromString :: String -> Maybe Focus
fromString = fst . split


-- | Parsing

separators :: String
separators = " .,;:|-_"

split :: String -> (Maybe Focus, String)
split str = split' $ parse str where
    split' [] = (Nothing, str)
    split' ((f, (s:ss)):_) | s `elem` separators = (Just f, ss)
    split' ((f, s):_) = (Just f, s)

strip :: String -> String
strip = snd . split

parse :: ReadS Focus
parse s = [(Focus xs, rest) | (xs, rest) <- parseInts s]


-- | Parser helpers

parseInts :: ReadS [Int]
parseInts s = separated ++ spaced ++ single where
    separated = [(x : xs, rest) | (x, s')    <- parseInt s
                                , (_, s'')   <- parseSep s'
                                , (xs, rest) <- parseInts s'']
    spaced    = [(x : xs, rest) | (x, s')    <- parseInt s
                                , (xs, rest) <- parseInts s' ]
    single    = [([x], rest)    | (x, rest)  <- parseInt s   ]

parseInt :: ReadS Int
parseInt s = [(x, rest'++rest) | (tok, rest) <- dotlex s
                               , (x, rest')  <- reads tok]

parseSep :: ReadS String
parseSep s = [(sep, rest) | (sep, rest) <- dotlex s
                          , sep `elem` map return separators]

-- A version of 'lex' that splits on dots as well, allowing us to parse
-- something like "1.2.3" as multiple numbers
dotlex :: ReadS String
dotlex = dotlex' . lstrip where
    dotlex' ('.' : s) = [(".", s)]
    dotlex' s = [(a, b++rest) | (tok, rest) <- lex s
                              , let (a:bs) = splitOn "." tok
                              , let b = intercalate "." ("":bs)]
