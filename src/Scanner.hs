{-# LANGUAGE MultiParamTypeClasses #-}
module Scanner where

import Data.Convertible
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC

import Scale.Types

type NodeAddress = String
type Node = (DepReq, NodeAddress)

instance Convertible DepReq SqlValue where
  safeConvert = Right . toSql . show

-- TODO: Error handling

initDatabase :: IO ()
initDatabase = withScanner $ \s -> do
  run s "CREATE TABLE scanner (depreq VARCHAR, node VARCHAR)" []
  return ()

withScanner :: (Connection -> IO a) -> IO a
withScanner fn = do
  conn <- connectSqlite3 "scanner.db"
  r <- fn conn
  commit conn
  disconnect conn
  return r

registerNode :: Node -> IO ()
registerNode (p, n) = withScanner $ \s -> do
  run s "INSERT INTO scanner VALUES (?, ?)" [toSql p, toSql n]
  return ()

unregisterNode :: NodeAddress -> IO ()
unregisterNode n = withScanner $ \s -> do
  run s "DELETE FROM scanner WHERE node = ?" [toSql n]
  return ()

queryNodes :: DepReq -> IO [NodeAddress]
queryNodes p = withScanner $ \s ->
  map (fromSql . head) <$> quickQuery' s "SELECT node FROM scanner WHERE depreq = ?" [toSql p]

dumpDB :: IO [String]
dumpDB = withScanner $ \s ->
  concatMap (map fromSql) <$> quickQuery' s "SELECT node, depreq FROM scanner" []

data Containers = Exec | Rkt

deploy Exec = undefined
-- scp executable, write systemd script and enable

deploy Rkt = undefined
-- scp container, enable through systemd

deployApp :: (String, DepReq) -> IO ()
deployApp (fname, q) = do
  nodes <- queryNodes q
  -- autodetect type of p
  case nodes of
    n:ns -> deploy Exec fname (head nodes)
    []   -> fail "No nodes in the registry satisfy deployment requirements"

