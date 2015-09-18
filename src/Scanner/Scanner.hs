{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main where

import Data.Convertible
import Data.Data
import Data.Typeable
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC
import Data.Char
import Data.Maybe
import Control.Monad
import Options.Applicative
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Posix.Process
import Text.Regex.Posix

import Scale.Types
import Paths_scanner

type NodeAddress = String
data Node = Node { host :: NodeAddress, caps :: DepReq, driver :: String }
  deriving (Show, Data, Typeable)

instance Convertible DepReq SqlValue where
  safeConvert = Right . toSql . show

-- TODO: Error handling

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

data Containers = RunLocal | Exec | Rkt

escapeQuotes [] = []
escapeQuotes ('"':cs) = '\\':'"':escapeQuotes cs
escapeQuotes (c:cs) = c:escapeQuotes cs

deploy RunLocal fname _ _ = do
  executeFile "runhaskell" True [fname] Nothing
  return undefined

deploy Exec fname n d = do
  -- TODO: write systemd script and enable
  putStrLn $ "sshpass -p scaffold scp " ++ fname ++ " root@" ++ n ++ ":~"
  runInteractiveCommand $ "sshpass -p scaffold scp " ++ fname ++ " root@" ++ n ++ ":~"
  print d
  (_, _, _, ph) <- runInteractiveCommand $ "sshpass -p scaffold ssh root@" ++ n ++ " /usr/bin/runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\""
  putStrLn $ "sshpass -p scaffold ssh root@" ++ n ++ " /usr/bin/runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\""
  return ph

deploy Rkt _ _ _ = undefined
-- scp container, enable through systemd

deployApp tgt prog = do
  dirfiles <- getDirectoryContents $ fst (splitFileName prog)
  let images = filter (=~ (prog ++ "[0-9][0-9]*")) dirfiles
  phs <- forM images $ \fname -> do
    putStrLn fname
    q :: DepReq <- fmap (read . unwords . tail . words . head . lines) . readFile $ fname
    nodes <- queryNodes q
    -- TODO: autodetect type of p
    ph <- case nodes of
      n:d:ns -> deploy tgt fname n ("[(" ++ (show q) ++ "," ++ (show d) ++ ")]")
      []   -> fail "No nodes in the registry satisfy deployment requirements"
    return ph
  waitForProcess (last phs)

parseRegisterArgs = Node <$>
      strOption (short 'h' <> long "host" <> metavar "<ip>" <> help "Node IP to register")
  <*> (   Provides <$> strOption (short 's' <> long "sense" <> metavar "<query>" <> help "Sensing capability to register")
      <|> IsCapableOf <$> strOption (short 'a' <> long "actuate" <> metavar "<actuator>" <> help  "Actuation capability to register"))
  <*> strOption (short 'd' <> long "driver" <> metavar "<exec>" <> help "Driver to use")

main :: IO ()
main = do
  -- TODO: Replace with proper args fwk
  allArgs <- getArgs
  unless (not . null $ allArgs) (printUsage >> exitSuccess)
  let cmd:args = allArgs
  case map toLower cmd of
    "register"    -> case getParseResult (execParserPure (prefs idm) (info parseRegisterArgs (progDesc "")) args) of
                        Nothing -> printUsage >> exitSuccess
                        Just ra -> registerNode ra
    "unregister"  -> unregisterNode . unwords $ args
    "query"       -> (queryNodes . read . capitalize . unwords $ args) >>= print
    "deploy"      -> void . deployApp Exec . unwords $ args
    "deploylocal" -> void . deployApp RunLocal . unwords $ args
    "display"     -> dumpDB >>= print
    otherwise -> printUsage >> exitSuccess
  return ()
  where
    printUsage = putStrLn "Usage: scn (unregister|register|query|deploy|deployLocal) args"

    capitalize [] = []
    capitalize (c:cs) = (toUpper c:map toLower cs)

