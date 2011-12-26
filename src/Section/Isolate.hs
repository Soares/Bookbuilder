module Section.Isolate
    ( Isolate
    , name
    , body
    , path
    , isSameGender
    , focus
    , create ) where
import Control.Applicative
import Data.Focus ( Focus )
import Data.Function ( on )
import qualified Data.Focus as Focus
import Data.Maybe
import System.Directory
import System.FilePath

data Isolate = Isolate { _name :: String
                       , _body :: Maybe String
                       } deriving Show

name :: Isolate -> String
name = _name

body :: Isolate -> Maybe String
body = _body

path :: FilePath -> Isolate -> FilePath
path dir = (dir </>) . _name

isSameGender :: Isolate -> Isolate -> Bool
isSameGender = (==) `on` (isJust . _body)

focus :: Isolate -> Focus
focus = fromMaybe Focus.unfocused . Focus.fromString . name

create :: FilePath -> String -> IO Isolate
create dir file = do
    let full = dir </> file
    isDir <- doesDirectoryExist full
    b <- if isDir then return Nothing else Just <$> readFile full
    return $ Isolate file b
