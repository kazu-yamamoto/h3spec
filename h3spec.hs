module Main where

import Control.Monad
import System.Environment
import System.Exit
import qualified Test.Hspec.Core.Runner as H

import Network.QUIC
import Transport

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ do
        putStrLn "Usage: h3spec <host> <port>"
        exitFailure
    let [host,port] = args
        cc = defaultClientConfig {
            ccServerName = host
          , ccPortName   = port
          }
    withArgs [] (H.runSpec (transportSpec cc) H.defaultConfig) >>= H.evaluateSummary
