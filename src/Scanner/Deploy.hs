{-# LANGUAGE ScopedTypeVariables #-}
module Scanner.Deploy where

import Control.Monad
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Posix.Process
import Text.Regex.Posix

import Scaffold.Types hiding (Node)
import Scanner.Query

-- TODO: Error handling

data Containers = RunLocal | Exec | Rkt

escapeQuotes [] = []
escapeQuotes ('"':cs) = '\\':'"':escapeQuotes cs
escapeQuotes (c:cs) = c:escapeQuotes cs

deploy RunLocal fname _ _ _ _ = do
  executeFile "runhaskell" True [fname] Nothing
  return undefined

deploy Exec fname u n d all = do
  -- TODO: write systemd script and enable
  putStrLn $ "sshpass -p scaffold scp " ++ fname ++ " " ++ u ++ "@" ++ n ++ ":~"
  runInteractiveCommand $ "sshpass -p scaffold scp " ++ fname ++ " " ++ u ++ "@" ++ n ++ ":~"
  print d
  (_, oh, eh, ph) <- runInteractiveCommand $ "sshpass -p scaffold ssh " ++ u ++ "@" ++ n ++ " -- /bin/bash -l runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\" \"'" ++ (escapeQuotes . show $ all) ++ "'\""
  putStrLn $ "sshpass -p scaffold ssh " ++ u ++ "@" ++ n ++ " -- /bin/bash -l runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\" \"'" ++ (escapeQuotes . show $ all) ++ "'\""
  return ((oh,eh),ph)

deploy Rkt _ _ _ _ _ = undefined
-- scp container, enable through systemd

deployApp mr tgt prog opts = do
  dirfiles <- getDirectoryContents $ fst (splitFileName prog)
  let images = filter (=~ (prog ++ "[0-9][0-9]*")) dirfiles
  -- FIXME: Very ugly.
  ohphs <- forM (zip images [0..length images]) $ \(fname, i) -> do
    putStrLn fname
    q :: DepReq <- fmap (read . unwords . tail . words . head . lines) . readFile $ fname
    qs :: [DepReq] <- fmap (read . unwords . tail . words . head . tail . lines) . readFile $ fname
    allNodes <- forM qs $ \q' -> do
      nodes <- queryNodes mr q'
      case nodes of
        n':_ -> return (q', n')
        []     -> fail "No nodes in the registry satisfy deployment requirements"
    nodes <- queryNodes mr q
    -- TODO: autodetect type of p
    ((oh,eh),ph) <- case nodes of
      n:_:d:u:ns -> deploy tgt fname u n ("[(" ++ (show q) ++ "," ++ (show d) ++ ")]") allNodes
      []     -> fail "No nodes in the registry satisfy deployment requirements"
    return ((oh,eh),ph)
  putStr =<< (hGetContents . fst . fst . last $ ohphs)
  putStr =<< (hGetContents . snd . fst . last $ ohphs)
  -- TODO: Wait for all the ghc processes to start
  waitForProcess (snd $ last ohphs)

