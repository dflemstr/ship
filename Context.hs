module Context where

import Control.Concurrent.MVar (MVar)

import Control.Monad.Reader (ReaderT)

import Data.Time.Clock (DiffTime)

import System.Time.Monotonic (Clock)

import Config (Config)
import Ship (Ships)
import WebSockets (WsClients)

data Context =
  Context
  { contextConf :: Config
  , contextShipsVar :: MVar Ships
  , contextWsClientsVar :: MVar WsClients
  , contextClock :: Clock
  , contextStartTime :: DiffTime
  }

type ContextT = ReaderT Context

type ContextIO = ContextT IO
