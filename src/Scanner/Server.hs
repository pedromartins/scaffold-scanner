module Main where

import Scaffold.Register
import Scaffold.Types

import Scanner.DBC
import Scanner.Deploy
import Scanner.Query
import Scanner.Register

import Network.XmlRpc.Server

hello :: Int -> IO String
hello _ = return "Hello"

-- Don't forget to check the permissions on scanner.db and the parent dir.
main = cgiXmlRpcServer [ ("register", fun registerNode)
                       , ("unregister", fun unregisterNode)
                       , ("query", fun queryNodesDB) -- . read . capitalize
                       , ("display", fun dumpNodes)
                       , ("hello", fun hello)
                       ]

