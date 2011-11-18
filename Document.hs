module Document where

import Control.Monad.Trans (liftIO)
import Control.Monad ((>=>), when)
import Control.Monad.Reader (runReaderT, asks)
import Text.Pandoc (Pandoc(Pandoc), Meta)

import Utils (withMeta)
import Context (Context, Contextual)
import qualified Context

-- Control.Monad utilities
find :: (Context -> IO a) -> Contextual IO a
find = asks >=> liftIO

data Document = Document { contents :: Pandoc
						             , isDirty :: Bool }
              deriving (Eq, Ord, Show, Read)

doDocument :: Contextual IO Document
doDocument = find Context.isFile >>= handle where
	handle True = doDocumentFile
	handle False = doDocumentDir

doDocumentFile :: Contextual IO Document
doDocumentFile = do
	-- Oi context, how do we do the stuff?
	load <- asks Context.loader
	process <- asks Context.processor
	output <- asks Context.outputer
	head <- asks Context.meta
	dirty <- find Context.isDirty
	-- Read the data
	raw <- liftIO load
	let pandoc = process raw `withMeta` head
	-- Output the data
	liftIO $ when dirty $ output pandoc
	-- Pass it on
	return $ Document pandoc dirty

doDocumentDir :: Contextual IO Document
doDocumentDir = do
	-- Oi context, how do we do the stuff?
	fragments <- find Context.fragments
	join <- asks Context.joiner
	output <- asks Context.outputer
	head <- asks Context.meta
	next <- asks Context.descend
	-- Oi context, descend and run on those kids
	let doFragment = runReaderT doDocument . next
	sections <- mapM (liftIO . doFragment) fragments
	-- All right, lump those kids together
	let dirty = any isDirty sections
    -- Todo: pointfree this lambda
	let pandoc = foldl (\p s -> p `join` contents s) (Pandoc head []) sections
	-- Output the data
	liftIO $ when dirty $ output pandoc
	-- Pass it on
	return $ Document pandoc dirty
