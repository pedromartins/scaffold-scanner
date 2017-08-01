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

module Scanner.DBC where

import Control.Applicative
import Data.Convertible
import Data.Data
import Data.Typeable
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import System.Process
import System.Directory

import Paths_scanner

import Scaffold.Types hiding (Node)
import Scaffold.Util

import Scaffold.Config

initDatabase :: IO ()
initDatabase = withScanner $ \s -> do
  run s "CREATE TABLE IF NOT EXISTS node (\
            \id INTEGER PRIMARY KEY,\
            \host VARCHAR\
        \)" []
  run s "CREATE TABLE IF NOT EXISTS node_cap (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \node INTEGER,\
            \depreq VARCHAR, driver VARCHAR, user VARCHAR,\
            \FOREIGN KEY(node) REFERENCES node(id)\
        \)" []
  run s "DELETE FROM node" []
  run s "DELETE FROM node_cap" []
  return ()

withScanner :: (Connection -> IO a) -> IO a
withScanner fn = do
  dbpath <- readScannerConfig
  runInteractiveCommand $ "touch " ++ dbpath
  conn <- connectSqlite3 dbpath
  r <- fn conn
  commit conn
  disconnect conn
  return r

registerNodeRecord :: NodeCapRecord -> IO NodeId
registerNodeRecord (NodeCapRecord h c d u)  = withScanner $ \s ->
  withTransaction s $ \st -> do
    lid <- quickQuery' st "SELECT id FROM node WHERE host = ?" [toSql h]
    let mid = listToMaybe . concat $ lid
    newid <- case mid of
      Nothing -> do
        -- Yes, this means we start from 1
        maxid <- (head . concatMap (map fromSql) <$>
          quickQuery' st "SELECT COALESCE(MAX(id),0) FROM node" [] :: IO Int)
        let newid = succ maxid
        run st "INSERT INTO node(id, host) SELECT ?, ?" [toSql newid, toSql h]
        -- run s "INSERT INTO node_users (?, ?)" [toSql h, toSql u]
        return newid
      Just id -> return (fromSql id)
    run st "INSERT INTO node_cap(node, depreq, driver, user) SELECT ?, ?, ?, ? \
           \WHERE NOT EXISTS (SELECT 1 from node_cap WHERE node = ? AND depreq = ? AND driver = ? AND user = ?)"
          (concat $ replicate 2 [toSql newid, toSql (show c), toSql d, toSql u])
    return newid
  where listToMaybe [] = Nothing
        listToMaybe (x:_) = Just x

unregisterNodeDB :: NodeAddress -> IO ()
unregisterNodeDB n = withScanner $ \s -> do
  id <- (head . concatMap (map fromSql) <$>
    quickQuery' s "SELECT * FROM node where host = ?" [toSql n] :: IO Int)
  run s "DELETE FROM node WHERE id = ?" [toSql id]
  run s "DELETE FROM node_cap where node = ?" [toSql id]
  return ()

queryNodesDB :: DepReq -> IO [String]
queryNodesDB Any = withScanner $ \s ->
  concatMap (map fromSql) <$>
    quickQuery' s "SELECT host, depreq, driver, user FROM node, node_cap WHERE node_cap.node = node.id" []

queryNodesDB p = withScanner $ \s -> do
  print (toSql (show p))
  concatMap (map fromSql) <$>
    quickQuery' s "SELECT host, depreq, driver, user FROM node, node_cap WHERE node_cap.node = node.id AND depreq = ?" [toSql (show p)]

dumpDB :: IO [[String]]
dumpDB = withScanner $ \s ->
  map (map fromSql) <$> quickQuery' s "SELECT host, depreq, driver, user FROM node, node_cap WHERE node_cap.node = node.id" []

