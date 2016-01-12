{-# LANGUAGE CPP #-}
module Scanner.Register where

import Data.Maybe
import Control.Monad
import Scanner.DBC
import Scanner.Types
import System.Directory
import System.IO
import System.Process
import Network.Socket
import qualified Data.Configurator as Cfg
import qualified Data.Text as T

import Scale.Types

type Attribute = (String, String)

-- STUB
queryCpu :: IO Attribute
queryCpu = return ("cpu", "N/A")

queryHD :: IO Attribute
queryHD = return ("HD", "N/A")

readSensorConfig :: FilePath -> IO (String, String)
readSensorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "sensor.query")
  driver <- Cfg.lookup cfg (T.pack "sensor.driver")
  return (fromJust query, fromJust driver)

readActuatorConfig :: FilePath -> IO (String, String)
readActuatorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "actuator.command")
  driver <- Cfg.lookup cfg (T.pack "actuator.driver")
  return (fromJust query, fromJust driver)

getFilesRec d = do
  nodes <- listDirectory d
  fmap concat . forM nodes $ \n -> do
    idn <- isDirectory n
    if idn then getFilesRec n else return [n]

-- TODO: Change so that the files can be anywhere within .scaffold.d
readConfig = do
  homeDirectory <- getHomeDirectory
  sensorPaths <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/sensors"
  actuatorPaths <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/actuators"
  sensorFiles <- filterM isFile . map ((homeDirectory ++ "/.scaffold.d/sensors/") ++) $ sensorPaths
  actuatorFiles <- filterM isFile . map ((homeDirectory ++ "/.scaffold.d/actuators/") ++) $ actuatorPaths
  sensors <- mapM readSensorConfig sensorFiles
  actuators <- mapM readActuatorConfig actuatorFiles
  return (sensors, actuators)

registerNode (NodeCapRecord Nothing c d u) = do
  ip <- getIp
  (sensors, actuators) <- readConfig
  mapM (\(s,sd) -> registerNodeRecord (NodeCapRecord (Just ip) (Provides s) sd u)) sensors
  mapM (\(c,sc) -> registerNodeRecord (NodeCapRecord (Just ip) (IsCapableOf c) sc u)) actuators
  registerNodeRecord (NodeCapRecord (Just ip) c d u)
registerNode ncr@(NodeCapRecord (Just _) _ _ _) = registerNodeRecord ncr

unregisterNode Nothing = getIp >>= unregisterNodeDB
unregisterNode (Just n) = unregisterNodeDB n

-- Copied from directory-1.2.5.0 to avoid dependency hell
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    (filter f) <$> (getDirectoryContents path)
      where f filename = filename /= "." && filename /= ".."

isFile = doesFileExist
isDirectory = fmap not . doesFileExist

-- More NAT friendly. Should be replaced with proper socket code.
getIp = do
#ifdef darwin_HOST_OS
  (_, oh, _, _) <- runInteractiveCommand "ifconfig `route get www.doc.ic.ac.uk | grep interface | cut -d ':' -f 2` | grep 'inet ' | cut -d ' ' -f 2"
#else
  (_, oh, _, _) <- runInteractiveCommand "ip route get 8.8.8.8 | head -n 1 | awk '{print $NF}'"
#endif
  fmap init $ hGetContents oh

