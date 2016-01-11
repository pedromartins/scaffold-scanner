module Main where

import Scanner.Types
import Scanner.DBC
import Scanner.Register
import Scanner.Deploy
import Scanner.Query

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

