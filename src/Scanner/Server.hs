-- Copyright 2017 Pedro M. N. Martins, Julie A. McCann, Imperial College London 
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice, this
-- list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its contributors
-- may be used to endorse or promote products derived from this software without
-- specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Main where

import Control.Concurrent
import Data.Char

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import System.IO

import Scaffold.Config
import Scaffold.Register
import Scaffold.Types

import Scanner.DBC
import Scanner.Deploy
import Scanner.Query
import Scanner.Register

import Network.XmlRpc.Server

import qualified Data.ByteString.Lazy as LB

hello :: Int -> IO String
hello _ = return "Hello"

xmlRPCServer :: [(String, XmlRpcMethod)] -> IO ()
xmlRPCServer meths = do
  port <- readPortConfig
  serverWith (Config stdLogger "0.0.0.0" (fromInteger port)) $ \_ _ req -> do
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

