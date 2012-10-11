{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebSockets where

import Data.Aeson (object, ToJSON(..), (.=))

import Data.Data (Data, Typeable)

import Data.Function (on)

import Data.IxSet (Indexable(..), ixFun, ixSet, IxSet)

import Data.Lens.Template (makeLenses)

import Data.Ord (comparing)

import Data.Text (Text)

import Network.WebSockets (Hybi00, Sink)

newtype WsClientId =
  WsClientId
  { unWsClientId :: Integer
  }
  deriving (Eq, Ord, Num, Data, Typeable)

data WsClient =
  WsClient
  { _wsClientId :: WsClientId
  , _wsClientSink :: Sink Hybi00
  } deriving (Typeable)

instance Eq WsClient where
  (==) = (==) `on` _wsClientId

instance Ord WsClient where
  compare = comparing _wsClientId

instance Indexable WsClient where
  empty =
    ixSet
    [ ixFun $ return . _wsClientId
    ]

data WsClients =
  WsClients
  { _wsClients :: IxSet WsClient
  , _wsClientsNextId :: WsClientId
  }

makeLenses [''WsClient, ''WsClients]

data WsState a =
  WsState
  { wsStateName :: Text
  , wsStateValue :: a
  }

instance (ToJSON a) => ToJSON (WsState a) where
  toJSON (WsState name val) =
    object ["type" .= ("state" :: Text), "what" .= name, "data" .= val]

data WsEvent a =
  WsEvent
  { wsEventValue :: a
  }

instance (ToJSON a) => ToJSON (WsEvent a) where
  toJSON (WsEvent val) =
    object ["type" .= ("event" :: Text), "data" .= val]
