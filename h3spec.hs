{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (foldl', intersperse)
import Data.Version (showVersion)
import Network.QUIC
import System.Console.GetOpt
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Test.Hspec.Core.Runner as H

import Transport
import qualified Paths_h3_spec as P

data Options = Options {
    optVersion :: Bool
  , optMatches :: [String]
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optVersion = False
  , optMatches = []
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v'] ["version"]
    (NoArg (\o -> o { optVersion = True }))
    "Print version"
  , Option ['m'] ["match"]
    (ReqArg (\m o -> o { optMatches = m : optMatches o}) "<test case description>")
    "Select a test case"
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
    (opts,args) <- case getOpt Permute options args0 of
      (o,n,[]) -> return (foldl' (flip id) defaultOptions o, n)
      (_,_,errs) -> showUsageAndExit $ concat errs
    when (optVersion opts) $ do
        putStrLn $ "h3spec " ++ showVersion P.version
        exitSuccess
    when (length args /= 2) $ do
        showUsageAndExit ""
    let [host,port] = args
        cc = defaultClientConfig {
            ccServerName = host
          , ccPortName   = port
          , ccALPN       = makeProtos
          , ccConfig     = defaultConfig {
                confVersions = [Draft29,Draft32]
                }
          }
        qcArgs
          | null (optMatches opts) = []
          | otherwise              = "--match" : (intersperse "--match" $ reverse $ optMatches opts)
    H.readConfig H.defaultConfig qcArgs >>= withArgs [] . H.runSpec (transportSpec cc) >>= H.evaluateSummary

makeProtos :: Version -> IO (Maybe [ByteString])
makeProtos ver = return $ Just [h3X,hqX]
  where
    verbs = C8.pack $ show $ fromVersion ver
    h3X = "h3-" `BS.append` verbs
    hqX = "hq-" `BS.append` verbs
