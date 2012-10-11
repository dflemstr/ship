{-# LANGUAGE OverloadedStrings #-}
module WebSocketServer where

import Control.Concurrent.MVar (MVar, readMVar)

import Control.Monad (forever, forM_)
import Control.Monad.State.Lazy (modify, StateT)

import Data.Aeson (encode, ToJSON)

import Data.ByteString (ByteString)

import Data.Functor ((<$>))

import qualified Data.IxSet as IxSet (deleteIx, insert, toList)

import Data.Lens (focus, getL, (+=))

import Data.Text (Text)

import Network.WebSockets

import Util
import WebSockets

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
