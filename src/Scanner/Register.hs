module Scanner.Register where

import Scaffold.Types
import Scaffold.Register

import Scanner.DBC

-- TODO: Refactor, this is awful.
registerNode (NodeCapRecord Nothing c d u) = do
  ip <- getIp
  (sensors, actuators) <- readConfig
  mapM (\(s,sd,u') -> registerNodeRecord $ NodeCapRecord (Just ip) (Provides s) sd u') sensors
  mapM (\(c,sc,u') -> registerNodeRecord $ NodeCapRecord (Just ip) (IsCapableOf c) sc u') actuators
  registerNodeRecord (NodeCapRecord (Just ip) c d u)
registerNode ncr@(NodeCapRecord (Just _) _ _ _) = registerNodeRecord ncr

unregisterNode Nothing = getIp >>= unregisterNodeDB
unregisterNode (Just n) = unregisterNodeDB n

