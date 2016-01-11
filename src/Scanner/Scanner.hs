{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.Maybe
import Control.Monad
import Options.Applicative
import System.Environment
import System.Exit
import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Scale.Types hiding (Node)

import Scanner.Types
import Scanner.Register
import Scanner.Deploy
import Scanner.Query
import Scanner.DBC

data RemoteArgs a = RemoteArgs { registry :: Maybe String, payload :: a }

-- TODO: Abstract away adding the registry option.
parseArgs :: Parser a -> Parser (RemoteArgs a)
parseArgs p = RemoteArgs <$>
      ((fmap Just $ strOption (short 'r' <> long "registry" <> metavar "<host>" <> help "Registry to use, defaults to localhost"))
        <|> pure Nothing)
  <*> p

parseNodeCapRecord = NodeCapRecord <$>
      (strOption (short 'h' <> long "host" <> metavar "<ip>" <> help "Node IP to register") <|> pure "localhost")
  <*> (   Provides <$> strOption (short 's' <> long "sense" <> metavar "<query>" <> help "Sensing capability to register")
      <|> IsCapableOf <$> strOption (short 'a' <> long "actuate" <> metavar "<actuator>" <> help  "Actuation capability to register")
      <|> pure Any)
  <*> (strOption (short 'd' <> long "driver" <> metavar "<exec>" <> help "Driver to use")
      <|> pure "")
  <*> (strOption (short 'u' <> long "user" <> metavar "<username>" <> help "Username to use") <|> pure "")

rcommand :: forall a b. (XmlRpcType a, XmlRpcType b, Show b) => String -> (a -> IO b) -> Parser a -> Mod CommandFields (IO ())
rcommand c f p = command c (info (fmap handle (parseArgs p)) idm)
  where handle (RemoteArgs Nothing args) = (f >=> print) args
        handle (RemoteArgs (Just r) args) = ((remote r c :: a -> IO b) >=> print) args

processArgs = subparser
  (  (rcommand "register" registerNode parseNodeCapRecord)
  <> (rcommand "unregister" unregisterNode (argument str idm))
  <> (rcommand "query" (queryNodesDB . read . capitalize) (argument str idm))
  <> (command "deploy" (info (fmap (\(RemoteArgs mr args) ->
                                      case words args of
                                        (fname:opts) -> void $ deployApp mr Exec fname opts)
                             (parseArgs (argument str idm))) idm))
  <> (command "deployLocal" (info (fmap (\(RemoteArgs mr args) ->
                                          case words args of
                                            (fname:opts) -> void $ deployApp mr RunLocal fname opts)
                                  (parseArgs (argument str idm))) idm))
  <> (rcommand "display" dumpNodes (pure ()))
  <> (command "readconfig" (info (pure (readConfig >>= print)) idm))
  <> (command "init" (info (pure initDatabase) idm)))
  where
    capitalize [] = []
    capitalize (c:cs) = (toUpper c:map toLower cs)

main :: IO ()
main = join $ execParser (info processArgs idm)


