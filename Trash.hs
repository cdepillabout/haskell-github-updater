{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Prelude hiding (putStrLn)
--import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack, putStrLn)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
--import GHC.IO.Exception
import System.IO
--import System.IO.Error
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.Char (ord)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Text.ICU.Convert

import Data.Maybe

import GHC.Enum (fromEnum)

import qualified Data.ByteString.Lazy.UTF8 as UTF8ByteString
import qualified System.IO.UTF8 as UTF8IO

import Codec.Binary.UTF8.String

--jsonData :: UTF8ByteString.ByteString
--jsonData = "Hello日本語"
jsonData :: String
jsonData = "Hello日本語"

main :: IO ()
main = do
    --encoding <- hGetEncoding stdout
    --putStrLn $ show encoding
    --UTF8IO.hPutStrLn stdout jsonData
    --hPutStrLn stdout $ show $ map ord jsonData
    hSetEncoding stdout utf8
    putStrLn jsonData
    --UTF8IO.putStrLn $ UTF8ByteString.fromString jsonData

    myStreamHandler <- fmap withFormatter $ streamHandler stderr DEBUG
    let mylog = rootLoggerName
    updateGlobalLogger mylog (setLevel DEBUG)
    updateGlobalLogger mylog (setHandlers [myStreamHandler])
    debugM mylog $ "test logger: " ++ jsonData

    L.putStrLn $ L.pack jsonData
    putStrLn $ L.unpack $ L.pack jsonData

    putStrLn $ decodeString $ L.unpack $ L.pack $ encodeString jsonData

    --putStrLn $ decode $ L.pack jsonData

    putStrLn $ decodeString $ encodeString $ L.unpack $ L.pack jsonData

    putStrLn $ show $ map fromEnum jsonData
    putStrLn $ show $ map fromEnum $ encodeString jsonData
    --putStrLn $ show $ L.map (fromEnum) $ L.pack jsonData
    putStrLn $ show $ map fromEnum $ L.unpack $ L.pack jsonData

    --converter <- open "UTF8" $ Just True
    --TIO.putStrLn $ toUnicode converter $ (BI.unpackBytes . B.pack jsonData)
    --unpackBytes $ B.pack jsonData
    --show $ B.pack jsonData
    lalala $ L.pack jsonData

lalala waht = let who = L.uncons waht
              in case who of 
                    Nothing -> putStr ";"
                    Just (he, re) -> do
                        putStr ((show $ ord he) ++ " ")
                        lalala re


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time GitHub-Updater $prio] $msg"

