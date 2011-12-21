module Text.Bookbuilder.Book ( Book, base, discover, populate, flatten ) where

import Control.Monad.Reader ( asks )
import Control.Monad.Loops ( concatM )
import Control.Monad.Trans ( liftIO )
import Data.Functor ( (<$>) )
import Data.Maybe ( fromMaybe )
import Data.Tree ( Tree(Node) )
import Data.Tree.Zipper
	( TreePos
	, Empty
	, nextSpace
	, nextTree
	, insert
	, label
	, setLabel
	, tree
	, fromTree
	, parent
	, children )
import Text.Bookbuilder.Config ( Configged, Config )
import Text.Bookbuilder.Profile ( Profile )
import qualified Text.Bookbuilder.Section as Section
import Text.Bookbuilder.Section
	( Gender(..)
	, Structure
	, Section
	, gender
	, sections
	, above
	, valid
	, subsections
	, contextualize
	, reduce
	, fill
	, filepath )
import Text.Bookbuilder.Variables ( variables )

type Book = Structure

base :: Config -> IO Structure
base conf = (\s -> fromTree (Node s [])) <$> Section.base conf

discover :: Structure -> Configged IO Structure
discover z = do
	subsecs <- liftIO =<< asks (subsections $ sections z)
	filled <- concatM (map pipe subsecs) (children z)
	return $ fromMaybe z $ parent filled
	where pipe s hole = (maybe hole nextSpace) <$> (discover' s hole)

discover' :: Section -> TreePos Empty Section -> Configged IO (Maybe Structure)
discover' s z = let z' = insert (Node s []) z in do
	proceed <- asks $ valid $ sections z'
	if proceed then Just <$> discover z' else return Nothing

populate :: Structure -> Configged IO Book
populate z = let (Node s _) = tree z in do
	vars <- asks $ variables z
	s' <- case gender s of
		File -> do
			file <- asks . filepath $ sections z
			body <- liftIO $ readFile file
			return $ s `fill` body
		_ -> return s
	let z' = setLabel (contextualize s' vars) z
	fromMaybe z' <$> populate' (children z')

populate' :: TreePos Empty Section -> Configged IO (Maybe Structure)
populate' z = case nextTree z of
	Nothing -> return $ parent z
	Just c -> populate c >>= populate' . nextSpace

flatten :: Profile -> Book -> String
flatten prof z = reduce prof (s' : above z) where
	s' = if gender s == File then s else s `fill` body
	body = flatten' prof $ children z
	s = label z

flatten' :: Profile -> TreePos Empty Section -> String
flatten' prof z = case nextTree z of
	Just c -> (flatten prof c) ++ (flatten' prof $ nextSpace c)
	Nothing -> ""
