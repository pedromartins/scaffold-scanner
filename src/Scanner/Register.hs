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

getFilesRec d = do
  nodes <- listDirectory d
  fmap concat . forM nodes $ \n -> do
    idn <- isDirectory n
    if idn then getFilesRec n else return [n]

-- TODO: Change so that the files can be anywhere within .scaffold.d using tagged blocks
readConfig = do
  homeDirectory <- getHomeDirectory
  sensorPaths <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/sensors"
  actuatorPaths <- getDirectoryContents $ homeDirectory ++ "/.scaffold.d/actuators"
  sensorFiles <- filterM isFile . map ((homeDirectory ++ "/.scaffold.d/sensors/") ++) $ sensorPaths
  actuatorFiles <- filterM isFile . map ((homeDirectory ++ "/.scaffold.d/actuators/") ++) $ actuatorPaths
  sensors <- mapM readSensorConfig sensorFiles
  actuators <- mapM readActuatorConfig actuatorFiles
  return (sensors, actuators)

registerNode = registerNodeRecord

unregisterNode = unregisterNodeDB

-- Copied from directory-1.2.5.0 to avoid dependency hell
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    (filter f) <$> (getDirectoryContents path)
      where f filename = filename /= "." && filename /= ".."

isFile = doesFileExist
isDirectory = fmap not . doesFileExist

