module Scanner.DBC where

import Data.Convertible
import Data.Data
import Data.Typeable
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)

import Paths_scanner

import Scale.Types hiding (Node)
import Scale.Util
import Scanner.Types

initDatabase :: IO ()
initDatabase = withScanner $ \s -> do
  -- TODO: Do ids and fks
  run s "CREATE TABLE node (\
            \id INTEGER PRIMARY KEY,\
            \host VARCHAR\
        \)" []
  run s "CREATE TABLE node_cap (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \node INTEGER,\
            \depreq VARCHAR, driver VARCHAR, user VARCHAR,\
            \FOREIGN KEY(node) REFERENCES node(id)\
        \)" []
  return ()

withScanner :: (Connection -> IO a) -> IO a
withScanner fn = do
  sfname <- getDataFileName "scanner.db"
  conn <- connectSqlite3 sfname
  r <- fn conn
  commit conn
  disconnect conn
  return r

registerNodeRecord :: NodeCapRecord -> IO NodeId
registerNodeRecord (NodeCapRecord h c d u)  = withScanner $ \s ->
  withTransaction s $ \st -> do
    -- Should check if node exists first

    -- Yes, this means we start from 1
    maxid <- (head . concatMap (map fromSql) <$>
      quickQuery' st "SELECT COALESCE(MAX(id),0) FROM node" [] :: IO Int)
    let newid = succ maxid
    run st "INSERT INTO node(id, host) VALUES (?, ?)" [toSql newid, toSql h]
    run st "INSERT INTO node_cap(node, depreq, driver, user) VALUES (?, ?, ?, ?)"
          [toSql newid, toSql c, toSql d, toSql u]
    -- run s "INSERT INTO node_users (?, ?)" [toSql h, toSql u]
    return newid

unregisterNodeDB :: NodeAddress -> IO ()
unregisterNodeDB n = withScanner $ \s -> do
  id <- (head . concatMap (map fromSql) <$>
    quickQuery' s "SELECT * FROM node where host = ?" [toSql n] :: IO Int)
  run s "DELETE FROM node WHERE id = ?" [toSql id]
  run s "DELETE FROM node_cap where id = ?" [toSql id]
  return ()

queryNodes :: DepReq -> IO [String]
queryNodes Any = withScanner $ \s ->
  concatMap (map fromSql) <$>
    quickQuery' s "SELECT host, depreq, driver FROM node, node_cap WHERE node_cap.node = node.id" []

queryNodes p = withScanner $ \s -> do
  print (toSql p)
  concatMap (map fromSql) <$>
    quickQuery' s "SELECT host, depreq, driver FROM node, node_cap WHERE node_cap.node = node.id AND depreq = ?" [toSql p]

dumpDB :: IO [[String]]
dumpDB = withScanner $ \s ->
  map (map fromSql) <$> quickQuery' s "SELECT host, depreq, driver FROM node, node_cap WHERE node_cap.node = node.id" []

