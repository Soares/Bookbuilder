module Section ( Section, load, flatten ) where
import Control.Applicative
import Control.Monad.Loops
import Data.Focus
import Data.Maybe
import Data.Scope
import Data.Tree hiding ( flatten )
import Data.Tree.Zipper
import System.Directory
import System.FilePath
import System.FilePath.Utils
import Section.Isolate ( Isolate, Gender(..) )
import qualified Section.Isolate as Isolate
import qualified Section.Info as Info
import qualified Path
import Text.Pandoc ( Pandoc )
import qualified Target.Pandoc as Pandoc

type Section = TreePos Full Isolate

load :: FilePath -> String -> Scope -> IO Section
load dir name scope = do
    i <- Isolate.create (dir </> name)
    discover dir scope (fromTree $ Node i [])


discover :: FilePath -> Scope -> Section -> IO Section
discover dir scope z = do
    subs <- subsections dir z
    discovered <- concatM (map pipe subs) (children z)
    return $ fromMaybe z $ parent discovered
    where pipe s hole = maybe hole nextSpace <$> discover' dir scope s hole
discover' :: FilePath -> Scope -> Isolate -> TreePos Empty Isolate -> IO (Maybe Section)
discover' dir scope s z = do
    let z' = insert (Node s []) z
    let proceed = scope `contains` (Info.location z')
    if proceed then Just <$> discover dir scope z' else return Nothing


type Renderer = (Pandoc -> String)
type Expander = (Focus -> [(String, String)] -> String)
flatten :: String -> Section -> Renderer -> Expander -> String
flatten t z render expand = expand loc vars where
    i = label z
    body = case Isolate.gender i of
        File text -> render $ Pandoc.parse (Isolate.name i) text
        Directory -> (flatten' render expand (children z)) ""
    vars = [("title", t), ("body", body)]
    loc = Info.location z
flatten' :: Renderer -> Expander -> TreePos Empty Isolate -> ShowS
flatten' render expand z = case nextTree z of
    Just c -> let
        t = Path.title $ Isolate.name $ label c
        ours = flatten t c render expand
        rest = flatten' render expand $ nextSpace c
        in (showString ours) . rest
    Nothing -> showString ""


subsections :: FilePath -> Section -> IO [Isolate]
subsections path z = do
    let dir = path </> (Isolate.name $ label z)
    there <- doesDirectoryExist dir
    if there then mapM Isolate.create =<< ls dir else return []
