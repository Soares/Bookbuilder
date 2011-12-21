module Text.Bookbuilder.Template.Constraint
	( Constraint
    , Constraints
	, load
    , includes
    , matches
	) where

import Control.Monad ( liftM2, join )
import Control.Applicative ( Applicative, (<*), (<$>) )
import Data.Limit ( Limit(Bounded, Unbounded) )
import Data.Function ( on )
import Text.ParserCombinators.Parsec

data Constraint = Constraint
	{ _order    :: Limit Int
	, _member   :: Int -> Bool }

type Constraints = Limit [Constraint]

-- NOTE: Constraint equality only measures whether constraints have equal
-- *weight* in terms of when they should be applied
-- Do *NOT* treat this as actual equality
instance Eq Constraint where (==) = (==) `on` _order
instance Ord Constraint where (<=) = (<=) `on` _order
instance Show Constraint where
	show (Constraint Unbounded _) = "@"
	show (Constraint _ fn) = concatMap (\x -> if fn x then "!" else ":") [1..7]



-- | Exported behavior              ====================================

load :: String -> Either ParseError Constraints
load = join $ parse $ try unconstrained <|> constrained

includes :: Constraint -> Int -> Bool
includes = _member

matches :: Constraints -> [Int] -> Bool
matches Unbounded _ = True
matches (Bounded xs) ys = if length xs == length ys then check xs ys else False
    where check cs zs = and $ zipWith includes cs zs



-- | Control extensions            =====================================

-- Execute through operators
(>>>) :: Applicative m => m a -> m b -> m a
(>>>) = (<*)

-- Combine operations
(+>>) :: Monad m => m a -> m b -> m (a, b)
(+>>) = liftM2 (,)



-- | Constraint utilities          =====================================

combine :: Constraint -> Constraint -> Constraint
combine = both (liftM2 (+)) (liftM2 (||)) where
	both aaa bbb (Constraint a1 b1) (Constraint a2 b2) = Constraint (aaa a1 a2) (bbb b1 b2)



-- | Universal constraints         =====================================

emptyConstraint, fullConstraint :: Constraint
emptyConstraint = Constraint (Bounded 0) (const False)
fullConstraint = Constraint Unbounded (const True)



-- | Parsing                       =====================================

-- | Top-level constraints
constrained, unconstrained :: GenParser Char st Constraints
constrained = fmap Bounded constraints
unconstrained = anymarker >> return Unbounded


-- | Constraint aggregation
constraints :: GenParser Char st [Constraint]
constraints = many constraint


-- | Constraint parsers
constraint, bounded, unbounded, nums, rng, lo, hi, single
    :: GenParser Char st Constraint

-- One level of constraint, unbounded if empty (1|4-6|10, etc.)
constraint = try bounded <|> unbounded
bounded = foldr combine emptyConstraint <$> sepBy1 nums pipe
unbounded = underscore >> return fullConstraint

-- A block of numbers
nums = try rng <|> try lo <|> try hi <|> single

-- A range of numbers bounded only at the bottom (3-, 7-, etc.)
lo = num >>> hyphen >>= \x -> return $ Constraint Unbounded (>= x)

-- A range of numbers bounded only at the top (-3, -7, etc.)
hi = hyphen >> num >>= \y -> return $ Constraint (Bounded $ 1 + y) (<= y)

-- A range of numbers (1-2, 3-17, etc.), from lower to higher
rng = do
	(x, y) <- num >>> hyphen +>> num
	let (x', y') = (min x y, max x y)
	return $ Constraint (Bounded $ 1 + y' - x') (`elem` [x'..y'])

-- An unadorned number (1, 2, etc.)
single = num >>= \n -> return $ Constraint (Bounded 1) (== n)


-- | Low-level parsed data
num :: GenParser Char st Int
num = fmap read $ many1 numchar


-- | Special strings
anymarker :: GenParser Char st String
anymarker = mapM char "any"


-- | Names for simple characters
hyphen, pipe, numchar, underscore :: GenParser Char st Char
hyphen = char '-'
pipe = oneOf "|"
underscore = char '_'
numchar = oneOf ['0'..'9']
