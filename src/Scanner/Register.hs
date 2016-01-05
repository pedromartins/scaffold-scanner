{-# LANGUAGE ScopedTypeVariables #-}
module Scanner.Register where

import Data.Maybe
import Control.Monad
import Scanner.DBC
import Scanner.Types
import System.Directory
import qualified Data.Configurator as Cfg
import qualified Data.Text as T

type Attribute = (String, String)

-- STUB
queryCpu :: IO Attribute
queryCpu = return ("cpu", "N/A")

queryHD :: IO Attribute
queryHD = return ("HD", "N/A")

readSensorConfig :: FilePath -> IO (String, String)
readSensorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "query")
  driver <- Cfg.lookup cfg (T.pack "driver")
  return (fromJust query, fromJust driver)

readActuatorConfig :: FilePath -> IO (String, String)
readActuatorConfig f = do
  cfg <- Cfg.load [ Cfg.Required f ]
  query <- Cfg.lookup cfg (T.pack "command")
  driver <- Cfg.lookup cfg (T.pack "driver")
  return (fromJust query, fromJust driver)

readConfig = do
  homeDirectory <- getHomeDirectory
  sensorPaths :: [FilePath] <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/sensors"
  actuatorPaths <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/actuators"
  sensorFiles <- filterM doesFileExist . map ((homeDirectory ++ "/.scaffold.d/sensors/") ++) $ sensorPaths
  actuatorFiles <- filterM doesFileExist . map ((homeDirectory ++ "/.scaffold.d/actuators/") ++) $ actuatorPaths
  sensors <- mapM readSensorConfig sensorFiles
  actuators <- mapM readActuatorConfig actuatorFiles
  return (sensors, actuators)

registerNode = registerNodeDB
