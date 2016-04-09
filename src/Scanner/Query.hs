module Scanner.Query where

import Network.XmlRpc.Client

import Scanner.DBC

queryNodes Nothing = queryNodesDB
queryNodes (Just r) = remote r "query"

dumpNodes () = dumpDB

