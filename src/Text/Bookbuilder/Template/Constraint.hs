module Text.Bookbuilder.Template.Constraint
	( Constraint(isMember)
	, fromName
	, check
	) where

import Control.Monad ( liftM2, join )
import Control.Applicative ( Applicative, (<*) )
import Data.Bound ( Bound(Bounded, Unbounded) )
import Data.Function ( on )
import Text.ParserCombinators.Parsec

-- | Constraint utilities          =====================================

data Constraint = Constraint
	{ order    :: Bound Integer
	, isMember :: Integer -> Bool }
-- NOTE: Constraint equality only measures whether constraints have equal
-- *weight* in terms of when they should be applied
-- Do *NOT* treat this as actual equality
instance Eq Constraint where (==) = (==) `on` order
instance Ord Constraint where (<=) = (<=) `on` order
-- For convenience & debugging only
instance Show Constraint where
	show (Constraint Unbounded _) = "@"
	show (Constraint _ fn) = concatMap (\x -> if fn x then "!" else ":") [1..7]



-- | Exported behavior              ====================================

fromName :: String -> Either ParseError (Bound [Constraint], String)
fromName = join $ parse bothParts where
	bothParts = (try unconstrained <|> constrained) +>> anything

check :: Bound [Constraint] -> [Integer] -> Bool
check Unbounded _ = True
check (Bounded xs) ys | length xs == length ys = check' xs ys
                      | otherwise = False where
    check' (g:gs) (z:zs) = isMember g z && check' gs zs
    check' _ _ = True



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

-- Top-level constraints
constrained, unconstrained :: GenParser Char st (Bound [Constraint])
constrained = fmap Bounded constraints
unconstrained = anymarker >> return Unbounded


-- Constraint aggregation
constraints :: GenParser Char st [Constraint]
constraints = endBy constraint underscore


-- Constraint parsers
constraint, nums, rng, lo, hi, single :: GenParser Char st Constraint

-- One level of constraint, unbounded if empty (1|4-6|10, etc.)
constraint = do
	chunks <- sepBy nums pipe
	return $ if null chunks
		then fullConstraint
		else foldr combine emptyConstraint chunks

-- A block of numbers
nums = try rng <|> try lo <|> try hi <|> single

-- A range of numbers bounded only at the bottom (3-, 7-, etc.)
lo = num >>> hyphen >>= \x -> return $ Constraint Unbounded (>= x)

-- A range of numbers bounded only at the top (-3, -7, etc.)
hi = hyphen >> num >>= \y -> return $ Constraint (Bounded $ 1 + y) (<= y)

-- A range of numbers (1-2, 3-17, etc.), from lower to higher
rng = do
	(x', y') <- num >>> hyphen +>> num
	let (x, y) = (min x' y', max x' y')
	return $ Constraint (Bounded $ 1 + y - x) (`elem` [x..y])

-- An unadorned number (1, 2, etc.)
single = num >>= \n -> return $ Constraint (Bounded 1) (== n)


-- Low-level parsed data
num :: GenParser Char st Integer
num = fmap read $ many1 numchar


-- Special strings
anything, anymarker :: GenParser Char st String
anything = many $ noneOf ""
anymarker = mapM char "any"


-- Names for simple characters
hyphen, pipe, numchar, underscore :: GenParser Char st Char
hyphen = char '-'
pipe = oneOf "|"
underscore = char '_'
numchar = oneOf ['0'..'9']
