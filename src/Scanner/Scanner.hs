{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.Maybe
import Control.Monad
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import System.Process
import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Scaffold.Types hiding (Node)

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
      ((Just <$> strOption (short 'h' <> long "host" <> metavar "<ip>" <> help "Node IP to register")) <|> pure Nothing)
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
  (_, oh, _, _) <- runInteractiveCommand "curl http://www.doc.ic.ac.uk/~pm1108/scaffold/dynIP"
  r <- hGetContents oh
  join $ execParser (info (helper <*> processArgs r) idm)

