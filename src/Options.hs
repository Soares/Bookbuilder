module Options ( Options(..), configure ) where

import Prelude hiding ( log )
import Control.Dangerous hiding ( Exit, Warning )
import Control.Monad
import Control.Monad.Loops
import Data.Focus
import Data.List
import System.Console.GetOpt
import Text.Printf


data Options = Options
    { optRoot      :: FilePath
    , optSourceDir :: FilePath
    , optTargetDir :: FilePath
    , optBuildDir  :: FilePath
    , optStart     :: Maybe Focus
    , optEnd       :: Maybe Focus
    , optTargets   :: [String]
    , optIsWithin  :: Bool
    , optDetect    :: Bool
    , optDebug     :: Bool
    , optHelp      :: Bool
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { optRoot      = ""
    , optSourceDir = "src"
    , optTargetDir = "targets"
    , optBuildDir  = "build"
    , optStart     = Nothing
    , optEnd       = Nothing
    , optTargets   = []
    , optIsWithin  = False
    , optDetect    = True
    , optDebug     = False
    , optHelp      = False }


-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Options -> Dangerous Options)]
options =
    [ Option [] ["src"]
        (ReqArg (\arg opt -> return opt{ optSourceDir = arg }) "DIR")
        "directory containing the book source files"

    , Option [] ["targets"]
        (ReqArg (\arg opt -> return opt{ optTargetDir = arg }) "DIR")
        "directory in which to place compiled files"

    , Option [] ["build"]
        (ReqArg (\arg opt -> return opt{ optBuildDir = arg }) "DIR")
        "directory in which to place compiled files"

    , Option "t" ["target"]
        (ReqArg (\arg opt -> return
            opt{ optTargets = arg : optTargets opt }) "PROF")
        "the target to be built (may be specified multiple times)"

    , Option "b" ["begin"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
            \foc -> return opt{ optStart = Just foc }) "LOC")
        "the section to start at, i.e. 01-02 or 2.3.1"

    , Option "e" ["end"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
            \foc -> return opt{ optEnd = Just foc }) "LOC")
        "the section to build up to, i.e. 4 or 02_04"

    , Option "n" ["only"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
            \foc -> return opt{ optStart = Just foc, optEnd = Just foc }) "LOC")
        "the specific section to compile"

    , Option [] ["nodetect"]
        (NoArg (\opt -> return opt{ optDetect = True }))
        "don't try to detect the book from the current location"

    , Option "d" ["debug"]
        (NoArg (\opt -> return opt{ optDebug = True }))
        "write the intermediary files, i.e. default.pdf.tex"

    , Option "h" ["help"]
        (NoArg (\opt -> return opt { optHelp = True}))
        "display this help"
    ]


parseLOC :: String -> Dangerous Focus
parseLOC arg = case parse arg of
    ((f, ""):_) -> return f
    _ -> throw $ CantParseLoc arg


addArgs :: [String] -> Options -> Dangerous Options
addArgs [] opts = return opts
addArgs [x] opts = return opts{ optRoot = x }
addArgs (x:xs) opts = mapM_ (warn . ExtraArg) xs >> addArgs [x] opts 


configure :: String -> [String] -> Dangerous Options
configure progname argv = do
    let (actions, args, unrecognized, errors) = getOpt' Permute options argv
    unless (null unrecognized) $
        mapM_ (warn . Unrecognized) unrecognized
    unless (null errors) $
        throw (OptErrors errors)
    config <- addArgs args =<< concatM actions defaultOptions
    when (optHelp config) $
        stop (DisplayHelp progname $ usageInfo progname options)
    return config


data Stop = DisplayHelp String String
instance Show Stop where
    show (DisplayHelp n s) = printf "%s: %s" n s

data Failure = OptErrors [String] | CantParseLoc String
instance Show Failure where
    show (OptErrors es) = "failed to parse options:\n" ++ intercalate "\t" es
    show (CantParseLoc input) = printf "can't parse location '%s'\n" input ++
        "\tLOCs must be given in the form of numbers separated '.' or '_'\n" ++
        "\texamples: 3_1_5 or 2.1.1.4"

data Warning = ExtraArg String | Unrecognized String
instance Show Warning where
    show (ExtraArg s) = "ignoring extra argument: " ++ s
    show (Unrecognized s) = "ignoring unrecognized flag: " ++ s
