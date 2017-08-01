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

{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.Maybe
import Data.Monoid hiding (Any)
import Control.Monad
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import System.Process
import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Scaffold.Types hiding (Node)
import Scaffold.Config
import Scaffold.Register
import Scaffold.Util
import Scaffold.Types

import Scanner.Register
import Scanner.Deploy
import Scanner.Query
import Scanner.DBC

data RemoteArgs a = RemoteArgs { registry :: Maybe String, payload :: a }

-- TODO: Abstract away adding the registry option.
parseArgs :: String -> Parser a -> Parser (RemoteArgs a)
parseArgs r p = RemoteArgs <$>
      ((fmap Just $ strOption (short 'r' <> long "registry" <> metavar "<host>" <> help "Registry to use, defaults to core."))
        <|> pure (Just r))
  <*> p

parseNodeCapRecord = NodeCapRecord <$>
      ((Just <$> strOption (short 'n' <> long "node" <> metavar "<ip>" <> help "Node IP to register")) <|> pure Nothing)
  <*> (   Provides <$> strOption (short 's' <> long "sense" <> metavar "<query>" <> help "Sensing capability to register")
      <|> IsCapableOf <$> strOption (short 'a' <> long "actuate" <> metavar "<actuator>" <> help  "Actuation capability to register")
      <|> pure Any)
  <*> (strOption (short 'd' <> long "driver" <> metavar "<exec>" <> help "Driver to use")
      <|> pure "")
  <*> (strOption (short 'u' <> long "user" <> metavar "<username>" <> help "Username to use") <|> pure "")

processArgs r = subparser
  (  (rcommand "register" registerNode parseNodeCapRecord)
  <> (rcommand "unregister" unregisterNode ((Just <$> argument str idm) <|> pure Nothing))
  <> (rcommand "query" (queryNodesDB . read . capitalize) (argument str idm))
  <> (command "deploy" (info (helper <*> fmap (\(RemoteArgs mr args) ->
                                      case words args of
                                        (fname:opts) -> void $ deployApp mr Exec fname opts)
                             (parseArgs r (argument str idm))) idm))
  <> (command "deployLocal" (info (helper <*> fmap (\(RemoteArgs mr args) ->
                                          case words args of
                                            (fname:opts) -> void $ deployApp mr RunLocal fname opts)
                                  (parseArgs r (argument str idm))) idm))
  <> (rcommand "display" dumpNodes (pure ()))
  <> (command "readconfig" (info (helper <*> pure (readConfig >>= print)) idm))
  <> (command "init" (info (helper <*> pure initDatabase) idm)))
  where
    capitalize [] = []
    capitalize (c:cs) = (toUpper c:map toLower cs)

    rcommand :: forall a b. (XmlRpcType a, XmlRpcType b, Show b) => String -> (a -> IO b) -> Parser a -> Mod CommandFields (IO ())
    rcommand c f p = command c (info (helper <*> fmap handle (parseArgs r p)) idm)
      where handle (RemoteArgs Nothing args) = (f >=> print) args
            handle (RemoteArgs (Just r) args) = ((remote r c :: a -> IO b) >=> print) args

main :: IO ()
main = do
  r <- getRegistry
  putStrLn $ "global registry:" ++ r
  join $ execParser (info (helper <*> processArgs r) idm)

