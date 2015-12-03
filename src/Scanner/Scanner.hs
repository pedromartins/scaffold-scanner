{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.Maybe
import Control.Monad
import Options.Applicative
import System.Environment
import System.Exit

import Scale.Types hiding (Node)

import Scanner.Types
import Scanner.DBC
import Scanner.Register
import Scanner.Deploy

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

