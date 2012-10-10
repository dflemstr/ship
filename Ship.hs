{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ship where

import Prelude hiding ((.), id)

import Control.Category

import Control.Concurrent

import Data.Aeson

import Data.Data

import Data.Function hiding ((.), id)

import Data.IxSet as IxSet

import Data.Lens
import Data.Lens.Template

import Data.Ord

import Data.Text (Text)

data ShipColor =
  ShipColor
  { _colorR :: !Int
  , _colorG :: !Int
  , _colorB :: !Int
  }
  deriving (Data, Typeable)

instance ToJSON ShipColor where
  toJSON (ShipColor r g b) =
    object ["r" .= r, "g" .= g, "b" .= b]

data ShipPosition =
  ShipPosition
  { _posX :: Double
  , _posY :: Double
  , _posHeading :: Double
  }
  deriving (Data, Typeable)

instance ToJSON ShipPosition where
  toJSON (ShipPosition x y heading) =
    object ["x" .= x, "y" .= y, "heading" .= heading]

data ShipStatus =
  ShipStatus
  { _statusEnergy :: !Double
  , _statusHealth :: !Double
  }
  deriving (Data, Typeable)

instance ToJSON ShipStatus where
  toJSON (ShipStatus energy health) =
    object ["energy" .= energy, "health" .= health]

data ShipState =
  ShipState
  { _stateShields :: !Bool
  , _stateBoost   :: !Bool
  }
  deriving (Data, Typeable)

instance ToJSON ShipState where
  toJSON (ShipState shields boost) =
    object ["shields" .= shields, "boost" .= boost]

newtype ShipId =
  ShipId
  { unShipId :: Integer
  }
  deriving (Eq, Ord, Num, Data, Typeable)

instance ToJSON ShipId where
  toJSON = toJSON . unShipId

data Ship =
  Ship
  { shipId :: !ShipId
  , shipThreadId :: !ThreadId
  , shipName :: !Text
  , _shipColor :: !ShipColor
  , _shipStatus :: !ShipStatus
  , _shipPosition :: !ShipPosition
  , _shipState :: !ShipState
  }
  deriving (Data, Typeable)

instance Eq Ship where
  (==) = (==) `on` shipId
instance Ord Ship where
  compare = comparing shipId

data Ships =
  Ships
  { _ships :: IxSet Ship
  , _shipsNextId :: ShipId
  }

instance ToJSON Ship where
  toJSON (Ship sid _ name color status position state) =
    object
    [ "id" .= sid
    , "name" .= name
    , "color" .= color
    , "status" .= status
    , "position" .= position
    , "state" .= state
    ]

instance Indexable Ship where
  empty =
    ixSet
    [ ixFun $ return . shipId
    , ixFun $ return . shipName
    ]

makeLenses
  [ ''ShipColor
  , ''ShipPosition
  , ''ShipStatus
  , ''ShipState
  , ''Ship
  , ''Ships
  ]

shipR :: Lens Ship Int
shipR = colorR . shipColor

shipG :: Lens Ship Int
shipG = colorG . shipColor

shipB :: Lens Ship Int
shipB = colorB . shipColor

shipX :: Lens Ship Double
shipX = posX . shipPosition

shipY :: Lens Ship Double
shipY = posY . shipPosition

shipHeading :: Lens Ship Double
shipHeading = posHeading . shipPosition

shipEnergy :: Lens Ship Double
shipEnergy = statusEnergy . shipStatus

shipHealth :: Lens Ship Double
shipHealth = statusHealth . shipStatus

shipShields :: Lens Ship Bool
shipShields = stateShields . shipState

shipBoost :: Lens Ship Bool
shipBoost = stateBoost . shipState
