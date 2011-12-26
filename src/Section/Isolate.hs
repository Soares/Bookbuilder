module Section.Isolate
    ( Isolate
    , name
    , body
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
                       , _body :: Maybe String }

name :: Isolate -> String
name = _name

body :: Isolate -> Maybe String
body = _body

isSameGender :: Isolate -> Isolate -> Bool
isSameGender = (==) `on` (isJust . _body)

focus :: Isolate -> Focus
focus = fromMaybe Focus.unfocused . Focus.fromString . name

create :: FilePath -> IO Isolate
create path = do
    isDir <- doesDirectoryExist path
    g <- if isDir then return Nothing else Just <$> readFile path
    return $ Isolate (takeFileName path) g
