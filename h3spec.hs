{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
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
          , ccALPN       = makeProtos
          , ccConfig     = defaultConfig {
                confVersions = [Draft29,Draft32]
                }
          }
    withArgs [] (H.runSpec (transportSpec cc) H.defaultConfig) >>= H.evaluateSummary

makeProtos :: Version -> IO (Maybe [ByteString])
makeProtos ver = return $ Just [h3X,hqX]
  where
    verbs = C8.pack $ show $ fromVersion ver
    h3X = "h3-" `BS.append` verbs
    hqX = "hq-" `BS.append` verbs
