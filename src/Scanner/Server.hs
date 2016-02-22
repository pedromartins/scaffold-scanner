module Main where

import Control.Concurrent
import Data.Char

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import System.IO

import Scaffold.Register
import Scaffold.Types

import Scanner.DBC
import Scanner.Deploy
import Scanner.Query
import Scanner.Register
import Scanner.Config

import Network.XmlRpc.Server

import qualified Data.ByteString.Lazy as LB

hello :: Int -> IO String
hello _ = return "Hello"

xmlRPCServer :: [(String, XmlRpcMethod)] -> IO ()
xmlRPCServer meths = do
  port <- readPortConfig
  serverWith (Config stdLogger "localhost" (fromInteger port)) $ \_ _ req -> do
    rsp <- fmap (map (chr . fromIntegral) . LB.unpack) $ handleCall (methods meths) (rqBody req)
    return $ Response (2,0,0) "" [Header HdrContentType "text/xml"
                                 ,Header HdrContentLength (show $ length rsp)]
                      rsp

-- Don't forget to check the permissions on scanner.db and the parent dir.
main = xmlRPCServer [ ("register", fun registerNode)
                    , ("unregister", fun unregisterNode)
                    , ("query", fun queryNodesDB) -- . read . capitalize
                    , ("display", fun dumpNodes)
                    , ("hello", fun hello)
                    ]

