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

import Scale.Types hiding (Node)
import Scanner.Types
import Scanner.DBC

-- TODO: Error handling

data Containers = RunLocal | Exec | Rkt

escapeQuotes [] = []
escapeQuotes ('"':cs) = '\\':'"':escapeQuotes cs
escapeQuotes (c:cs) = c:escapeQuotes cs

deploy RunLocal fname _ _ = do
  executeFile "runhaskell" True [fname] Nothing
  return undefined

deploy Exec fname n d = do
  -- TODO: write systemd script and enable
  putStrLn $ "sshpass -p scaffold scp " ++ fname ++ " scaffold@" ++ n ++ ":~"
  runInteractiveCommand $ "sshpass -p scaffold scp " ++ fname ++ " scaffold@" ++ n ++ ":~"
  print d
  (_, oh, eh, ph) <- runInteractiveCommand $ "sshpass -p scaffold ssh scaffold@" ++ n ++ " -- /bin/bash -l runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\""
  putStrLn $ "sshpass -p scaffold ssh scaffold@" ++ n ++ " -- /bin/bash -l runhaskell " ++ (snd $ splitFileName fname) ++ " \"'" ++ (escapeQuotes d) ++ "'\""
  return ((oh,eh),ph)

deploy Rkt _ _ _ = undefined
-- scp container, enable through systemd

deployApp tgt prog opts = do
  dirfiles <- getDirectoryContents $ fst (splitFileName prog)
  let images = filter (=~ (prog ++ "[0-9][0-9]*")) dirfiles
  ohphs <- forM (zip images [0..length images]) $ \(fname, i) -> do
    putStrLn fname
    q :: DepReq <- fmap (read . unwords . tail . words . head . lines) . readFile $ fname
    nodes <- queryNodes q
    -- TODO: autodetect type of p
    ((oh,eh),ph) <- case nodes of
      n:d:ns -> deploy tgt fname n ("[(" ++ (show q) ++ "," ++ (show d) ++ ")]")
      []     -> fail "No nodes in the registry satisfy deployment requirements"
    return ((oh,eh),ph)
  putStr =<< (hGetContents . fst . fst . last $ ohphs)
  putStr =<< (hGetContents . snd . fst . last $ ohphs)
  -- TODO: Wait for all the ghc processes to start
  waitForProcess (snd $ last ohphs)
