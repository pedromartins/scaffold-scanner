{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables #-}
module Scanner.Types where

import Data.Convertible
import Data.Data
import Database.HDBC

import Scale.Types hiding (Node)

type NodeAddress = String
data Node = Node { host :: NodeAddress, caps :: DepReq, driver :: String }
  deriving (Show, Data, Typeable)

instance Convertible DepReq SqlValue where
  safeConvert = Right . toSql . show


