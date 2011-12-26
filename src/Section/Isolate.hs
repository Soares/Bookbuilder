module Section.Isolate
    ( Gender(..)
    , Isolate
    , name
    , gender
    , focus
    , create ) where
import Control.Applicative
import Data.Focus ( Focus )
import qualified Data.Focus as Focus
import Data.Maybe
import System.Directory
import System.FilePath

data Gender = File String | Directory

data Isolate = Isolate { _name   :: String
                       , _gender :: Gender }

name :: Isolate -> String
name = _name

gender :: Isolate -> Gender
gender = _gender

focus :: Isolate -> Focus
focus = fromMaybe Focus.unfocused . Focus.fromString . name

create :: FilePath -> IO Isolate
create path = do
    isDir <- doesDirectoryExist path
    g <- if isDir then return Directory else File <$> readFile path
    return $ Isolate (takeFileName path) g

{-


section :: (FilePath, String) -> [(String, String)] -> IO Section
section (dir, name) vars = let path = dir </> name in do
    isDir <- doesDirectoryExist path
    let g = if isDir then Directory else File
    return Section{ _path = name, _context = vars, _gender = g, _body = "" }

base :: Config -> IO Section
base conf = section (src conf, "") (context conf)

gender :: Section -> Gender
gender = _gender

title :: Section -> Config -> String
title s c | _path s == "" = title' $ takeFileName $ root c
          | otherwise = title' $ _path s where
    title' = deCamel . dropExtension . stripLocation'
    deCamel = join " " . split (startsWithOneOf ['A'..'Z'])

index :: Section -> Location
index = fromMaybe nowhere . fromString . _path

above :: PosType t => TreePos t Section -> [Section]
above = map (\(_, s, _) -> s) . parents

sections :: TreePos Full Section -> [Section]
sections z = label z : above z

valid :: [Section] -> Config -> Bool
valid ss = isInRange (location ss)

filepath :: [Section] -> Config -> FilePath
filepath ss conf = src conf </> joinPath (reverse $ map _path ss)

location :: [Section] -> Location
location = foldr (focus . index) nowhere . reverse

reduce :: Profile -> [Section] -> String
reduce prof ss@(s:_) = render vars (template prof $ location ss) where
    vars = ("body", text $ gender s) : _context s
    text File = lift prof (_path s) (_body s)
    text Directory = _body s
reduce _ _ = ""

subsections :: [Section] -> Config -> IO [Section]
subsections [] conf = return <$> base conf
subsections (s:_) _ | gender s == File = return []
subsections ss conf = ls (filepath ss conf) >>= mapM create where
    create p = section (splitFileName p) (context conf)

contextualize :: Section -> [(String, String)] -> Section
contextualize s vars = s{ _context = vars ++ _context s }

fill :: Section -> String -> Section
fill s body = s{ _body = body }

decide :: String -> String -> Section -> Section
decide k v s = s{ _context = (k, v) : _context s }
    -}
