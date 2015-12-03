module Scanner.DBC where

import Data.Convertible
import Data.Data
import Data.Typeable
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)

import Paths_scanner

import Scale.Types hiding (Node)
import Scanner.Types

initDatabase :: IO ()
initDatabase = withScanner $ \s -> do
  run s "CREATE TABLE scanner (node VARCHAR, depreq VARCHAR, driver VARCHAR)" []
  return ()

withScanner :: (Connection -> IO a) -> IO a
withScanner fn = do
  sfname <- getDataFileName "scanner.db"
  conn <- connectSqlite3 sfname
  r <- fn conn
  commit conn
  disconnect conn
  return r

registerNode :: Node -> IO ()
registerNode (Node h c d)  = withScanner $ \s -> do
  run s "INSERT INTO scanner VALUES (?, ?, ?)" [toSql h, toSql c, toSql d]
  return ()

unregisterNode :: NodeAddress -> IO ()
unregisterNode n = withScanner $ \s -> do
  run s "DELETE FROM scanner WHERE node = ?" [toSql n]
  return ()

queryNodes :: DepReq -> IO [String]
queryNodes Any = withScanner $ \s -> do
  concatMap (map fromSql) <$> quickQuery' s "SELECT node, driver FROM scanner" []

queryNodes p = withScanner $ \s -> do
  print (toSql p)
  concatMap (map fromSql) <$> quickQuery' s "SELECT node, driver FROM scanner WHERE depreq = ?" [toSql p]

dumpDB :: IO [String]
dumpDB = withScanner $ \s ->
  concatMap (map fromSql) <$> quickQuery' s "SELECT node, depreq, driver FROM scanner" []
