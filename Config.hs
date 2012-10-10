{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Config where

import Data.Data

import Data.Default

import System.Log.Logger (Priority(..))

data Config =
  Config
  { logPriority :: Priority
  , movementUpdateFrequency :: Double
  , regenerationUpdateFrequency :: Double
  , arenaWidth :: Int
  , arenaHeight :: Int
  , serverPort :: Int
  , wsServerPort :: Int
  , shipRadius :: Double
  , shipScanRange :: Double
  , shipShootRange :: Double
  , shipSpeed :: Double
  , shipSpeedWithBoost :: Double
  , shipDamage :: Int
  , shipDamageWithShields :: Int
  , shipStartingEnergy :: Int
  , shipStartingHealth :: Int
  , shipMaxEnergy :: Int
  , shipMaxHealth :: Int
  , shipEnergyRegen :: Int
  , shipShieldEnergyCost :: Int
  , shipBoostEnergyCost :: Int
  , shipHealthRegen :: Int
  , scanCost :: Int
  , turnCost :: Int
  , shootCost :: Int
  } deriving (Data, Typeable)

deriving instance Data Priority
deriving instance Typeable Priority

instance Default Config where
  def =
    Config
    { logPriority = INFO
    , movementUpdateFrequency = 10
    , regenerationUpdateFrequency = 10
    , arenaWidth = 256
    , arenaHeight = 256
    , serverPort = 1337
    , wsServerPort = 1338
    , shipRadius = 8
    , shipScanRange = 32
    , shipShootRange = 64
    , shipSpeed = 32
    , shipSpeedWithBoost = 64
    , shipDamage = 50
    , shipDamageWithShields = 25
    , shipStartingEnergy = 25
    , shipStartingHealth = 75
    , shipMaxEnergy = 100
    , shipMaxHealth = 100
    , shipEnergyRegen = 1
    , shipShieldEnergyCost = 1
    , shipBoostEnergyCost = 1
    , shipHealthRegen = 2
    , scanCost = 5
    , turnCost = 5
    , shootCost = 20
    }
