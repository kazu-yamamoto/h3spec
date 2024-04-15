{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.List (foldl', intersperse)
import Data.Version (showVersion)
import qualified Network.HTTP3.Client as H3
import Network.QUIC.Internal
import System.Console.GetOpt
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Test.Hspec.Core.Runner as H

import HTTP3Error
import qualified Paths_h3spec as P
import TransportError

data Options = Options
    { optVersion :: Bool
    , optDebugLog :: Bool
    , optValidate :: Bool
    , optMatch :: [String]
    , optSkip :: [String]
    , optQLogDir :: Maybe FilePath
    , optKeyLogFile :: Maybe FilePath
    , optTimeout :: Int
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optVersion = False
        , optDebugLog = False
        , optValidate = True
        , optMatch = []
        , optSkip = []
        , optQLogDir = Nothing
        , optKeyLogFile = Nothing
        , optTimeout = 2000 -- 2 milliseconds
        }

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['v']
        ["version"]
        (NoArg (\o -> o{optVersion = True}))
        "Print version"
    , Option
        ['d']
        ["debug"]
        (NoArg (\o -> o{optDebugLog = True}))
        "print debug info"
    , Option
        ['m']
        ["match"]
        (ReqArg (\m o -> o{optMatch = m : optMatch o}) "<test case description>")
        "Select test cases"
    , Option
        ['s']
        ["skip"]
        (ReqArg (\m o -> o{optSkip = m : optSkip o}) "<test case description>")
        "Skip test cases"
    , Option
        ['q']
        ["qlog-dir"]
        (ReqArg (\dir o -> o{optQLogDir = Just dir}) "<dir>")
        "directory to store qlog"
    , Option
        ['l']
        ["key-log-file"]
        (ReqArg (\file o -> o{optKeyLogFile = Just file}) "<file>")
        "a file to store negotiated secrets"
    , Option
        ['t']
        ["timeout"]
        (ReqArg (\ms o -> o{optTimeout = read ms}) "<milliseconds>")
        "timeout for each test case (2000)"
    , Option
        ['n']
        ["no-validate"]
        (NoArg (\o -> o{optValidate = False}))
        "no validating server certificates"
    ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

usage :: String
usage = "Usage: h3spec <host> <port>"

main :: IO ()
main = do
    args0 <- getArgs
    (opts, args) <- case getOpt Permute options args0 of
        (o, n, []) -> return (foldl' (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs
    when (optVersion opts) $ do
        putStrLn $ "h3spec " ++ showVersion P.version
        exitSuccess
    (host, port) <- case args of
        [h, p] -> return (h, p)
        _ -> showUsageAndExit ""
    let cc =
            defaultClientConfig
                { ccServerName = host
                , ccPortName = port
                , ccALPN = \_ -> return $ Just ["h3", "h3-29", "hq-interop", "hq-29"]
                , ccDebugLog = optDebugLog opts
                , ccQLog = optQLogDir opts
                , ccKeyLog = getLogger $ optKeyLogFile opts
                , ccValidate = optValidate opts
                }
        qcArgs0
            | null (optMatch opts) = []
            | otherwise =
                "--match" : (intersperse "--match" $ reverse $ optMatch opts)
        qcArgs
            | null (optSkip opts) = qcArgs0
            | otherwise =
                "--skip" : (intersperse "--skip" $ reverse $ optSkip opts)
        h3cc = H3.ClientConfig "https" host
        ms = optTimeout opts
    H.readConfig H.defaultConfig qcArgs
        >>= withArgs [] . H.runSpec (transportErrorSpec cc ms >> h3ErrorSpec cc h3cc ms)
        >>= H.evaluateSummary

getLogger :: Maybe FilePath -> (String -> IO ())
getLogger Nothing = \_ -> return ()
getLogger (Just file) = \msg -> appendFile file (msg ++ "\n")
