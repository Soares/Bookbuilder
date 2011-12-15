module Text.Bookbuilder.Templates
	{-
	( Template(tmplName,tmplSource)
	, tmplMatches
	, isValid
	, parseGroups
	, loadTemplate ) where
	-}
	where

import Control.Monad ( liftM, liftM2, foldM )
import Text.ParserCombinators.Parsec

-- TODO: handle 'any' template
-- TODO: imports
-- TODO: import Data.Bounded

data Bound a = Bounded a | Unbounded
	deriving (Eq, Read, Show)

instance Ord a => Ord (Bound a) where
	x <= y = fromBound True $ liftM2 (<=) x y

instance Functor Bound where
	fmap _ Unbounded = Unbounded
	fmap f (Bounded a) = Bounded (f a)

instance Monad Bound where
	Unbounded >>= _ = Unbounded
	(Bounded x) >>= k = k x

	Unbounded >> _ = Unbounded
	(Bounded _) >> k = k

	return = Bounded
	fail _ = Unbounded

isBounded :: Bound a -> Bool
isBounded Unbounded = False
isBounded _ = True

isUnbounded :: Bound a -> Bool
isUnbounded = not . isBounded

fromBounded :: Bound a -> a
fromBounded Unbounded = error "Bound.fromBounded: Unbounded" -- Blast!
fromBounded (Bounded x) = x

fromBound :: a -> Bound a -> a
fromBound d Unbounded = d
fromBound d (Bounded x) = x
-- End bound

data Template = Template
	{ tmplName    :: String
	, tmplSource  :: String
	, tmplGroups  :: Bound [Group]
	}

type Group = (Bound Integer, Integer -> Bool)

instance Eq Template where
	t1 == t2 = tmplName t1 == tmplName t2

instance Ord Template where
	t1 <= t2 = orderings t1 <= orderings t2

orderings :: Template -> Bound [Bound Integer]
orderings = fmap (map fst) . tmplGroups

tmplMatches :: Template -> [Integer] -> Bool
tmplMatches = groupsMatch . tmplGroups

groupsMatch :: Bound [Group] -> [Integer] -> Bool
groupsMatch Unbounded _ = True
groupsMatch (Bounded (g:gs)) (x:xs) = snd g x && groupsMatch (Bounded gs) xs

combine :: Group -> Group -> Group
combine = both (liftM2 (+)) (liftM2 (||)) where
	both aaa bbb (a1, b1) (a2, b2) = (aaa a1 a2, bbb b1 b2)

emptyGroup = (Bounded 0, const False)

{-
readNameFor :: String -> [String] -> String -> Template
readNameFor theme exts name = template where
	template = Template 

	load _ = Template "name" "$body$" Unbounded
	resolve = groups >>> themeword
	themeword = do
		return theme
		dot
		choice $ map return exts
		-}

-- Execute through
(>>>) :: Monad m => m a -> m b -> m a
fst >>> snd = fst >>= (snd >>) . return

-- Operation combination
(+>>) :: Monad m => m a -> m b -> m (a, b)
fst +>> snd = fst >>= (\x -> snd >>= (\y -> return (x, y)))

constraints = try (groups >>= return . Bounded) <|> fb
fb = anystr >> hyphen >> return Unbounded

-- TODO: should be sepBy instead of endBy?
groups :: GenParser Char st [Group]
groups = endBy group hyphen

group, set, nums, rng, lo, hi, single :: GenParser Char st Group
group = try uni <|> set
set = fmap (foldr combine emptyGroup) (sepBy nums quote)
nums = try rng <|> try lo <|> try hi <|> single
lo = num >>> tilda >>= \x -> return (Unbounded, (>= x))
hi = tilda >> num >>= \y -> return (Bounded $ 1 + y, (<= y))
rng = do
	(x', y') <- num >>> tilda +>> num
	let (x, y) = (min x' y', max x' y')
	return (Bounded $ 1 + y - x, (`elem` [x..y]))
single = num >>= \n -> return (Bounded 1, (== n))
uni = many1 underscore >> return (Unbounded, const True)

-- | Simple data
num :: GenParser Char st Integer
num = fmap read $ many1 numchar

-- | Simple tokens
hyphen, tilda, quote, dot, numchar :: GenParser Char st Char
hyphen = char '-'
tilda = char '~'
quote = oneOf "'"
dot = char '.'
underscore = char '_'
numchar = oneOf ['0'..'9']
anystr = return "any"
