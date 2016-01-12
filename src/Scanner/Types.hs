{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
module Scanner.Types where

import Data.Convertible
import Data.Data
import Database.HDBC
import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType

import Scale.Types hiding (Node)

type NodeId = Int
type NodeAddress = String
data NodeCapRecord = NodeCapRecord { host :: Maybe NodeAddress, cap :: DepReq, driver :: String, user :: String }
  deriving (Show, Data, Typeable)

instance XmlRpcType DepReq where
  toValue = ValueString . show
  fromValue (ValueString s) = return $ read s
  getType _ = TUnknown

instance XmlRpcType () where
  toValue () = ValueInt 0
  fromValue _ = return ()
  getType _ = TUnknown

$(asXmlRpcStruct ''NodeCapRecord)

instance Convertible DepReq SqlValue where
  safeConvert = Right . toSql . show

instance (Show a, Read a) => XmlRpcType (Maybe a) where
  toValue = ValueString . show
  fromValue (ValueString x) = return $ read x
  getType _ = TUnknown
