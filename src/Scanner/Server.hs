module Main where

import Scanner.Types
import Scanner.DBC
import Scanner.Register
import Scanner.Deploy

import Network.XmlRpc.Server

main = cgiXmlRpcServer [ ("register", fun registerNode)
                       , ("unregister", fun unregisterNode)
                       , ("query", fun queryNodes) -- . read . capitalize
                       ]

