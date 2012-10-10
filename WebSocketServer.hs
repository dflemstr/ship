{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebSocketServer where

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Lazy

import Data.Aeson

import Data.ByteString (ByteString)

import Data.Data

import Data.Function

import Data.Functor

import Data.IxSet as IxSet

import Data.Lens
import Data.Lens.Template

import Data.Ord

import Data.Text (Text)

import Network.WebSockets

import Util

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

insertWsClient :: Monad m => Sink Hybi00 -> StateT WsClients m WsClient
insertWsClient sink = do
  clientId <- wsClientsNextId += 1
  let client = WsClient clientId sink
  focus wsClients . modify $ IxSet.insert client
  return client

deleteWsClient :: Monad m => WsClientId -> StateT WsClients m ()
deleteWsClient clientId =
  focus wsClients . modify $ IxSet.deleteIx clientId

runWebSocketServer :: Int -> MVar WsClients -> IO ()
runWebSocketServer port = runServer "0.0.0.0" port . webSocketApp

webSocketApp :: MVar WsClients -> Request -> WebSockets Hybi00 ()
webSocketApp clients rq = case requestPath rq of
  "/ships" -> do
    acceptRequest rq
    sink <- getSink
    client <- alterMVar clients . insertWsClient $ sink
    flip catchWsError (disconnect client) $
      forever (receiveData :: WebSockets Hybi00 ByteString)
  _ -> return ()
  where
    disconnect c _ = alterMVar clients . deleteWsClient . getL wsClientId $ c

wsTell :: ToJSON a => MVar WsClients -> a -> IO ()
wsTell clients a = do
  cs <- IxSet.toList . getL wsClients <$> readMVar clients
  forM_ cs $ flip sendSink msg . getL wsClientSink
  where
    msg = DataMessage . Text . encode $ a

wsTellState :: (ToJSON a) => MVar WsClients -> Text -> a -> IO ()
wsTellState clients name = wsTell clients  . WsState name

wsTellEvent :: (ToJSON a) => MVar WsClients -> a -> IO ()
wsTellEvent clients = wsTell clients . WsEvent
