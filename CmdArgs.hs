{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module CmdArgs where

import System.Console.CmdArgs.Implicit hiding (Default(..))

import Config

cmdConfFrom :: Config -> Config
cmdConfFrom def =
  Config
  { logPriority =
    logPriority def
    &= explicit
    &= help "log priority filter"
    &= name "priority"
  , movementUpdateFrequency =
    movementUpdateFrequency def
    &= explicit
    &= help "how often the ship positions should be updated"
    &= name "move-freq"
    &= name "m"
  , regenerationUpdateFrequency =
    regenerationUpdateFrequency def
    &= explicit
    &= help "how often the ship status levels should be updated"
    &= name "regen-freq"
    &= name "r"
  , arenaWidth =
    arenaWidth def
    &= explicit
    &= help "the width of the arena"
    &= name "width"
    &= name "w"
  , arenaHeight =
    arenaHeight def
    &= explicit
    &= help "the height of the arena"
    &= name "height"
    &= name "h"
  , serverPort =
    serverPort def
    &= explicit
    &= help "the port the TCP server runs on"
    &= name "port"
    &= name "p"
  , wsServerPort =
    wsServerPort def
    &= explicit
    &= help "the port the web sockets server runs on"
    &= name "ws-port"
  , shipRadius =
    shipRadius def
    &= explicit
    &= help "the radius of a ship (units)"
    &= name "ship-radius"
  , shipScanRange =
    shipScanRange def
    &= explicit
    &= help "the scan range of a ship (units)"
    &= name "ship-scan-range"
  , shipShootRange =
    shipShootRange def
    &= explicit
    &= help "the shooting range of a ship (units)"
    &= name "ship-shoot-range"
  , shipSpeed =
    shipSpeed def
    &= explicit
    &= help "the speed of a ship (units/second)"
    &= name "ship-speed"
  , shipSpeedWithBoost =
    shipSpeedWithBoost def
    &= explicit
    &= help "the speed of a boosted ship (units/second)"
    &= name "ship-boost-speed"
  , shipDamage =
    shipDamage def
    &= explicit
    &= help "the damage each shot by a ship does"
    &= name "ship-damage"
  , shipDamageWithShields =
    shipDamageWithShields def
    &= explicit
    &= help "the damage a ship does to a shielded ship"
    &= name "ship-shield-damage"
  , shipStartingEnergy =
    shipStartingEnergy def
    &= explicit
    &= help "a ship's starting energy"
    &= name "ship-starting-energy"
  , shipStartingHealth =
    shipStartingHealth def
    &= explicit
    &= help "a ship's starting health"
    &= name "ship-starting-health"
  , shipMaxEnergy =
    shipMaxEnergy def
    &= explicit
    &= help "a ship's max energy"
    &= name "ship-max-energy"
  , shipMaxHealth =
    shipMaxHealth def
    &= explicit
    &= help "a ship's max health"
    &= name "ship-max-health"
  , shipEnergyRegen =
    shipEnergyRegen def
    &= explicit
    &= help "the regeneration rate of a ship's energy per second"
    &= name "ship-energy-regen"
  , shipShieldEnergyCost =
    shipShieldEnergyCost def
    &= explicit
    &= help "how much the energy regeneration slows down when using shields"
    &= name "ship-shield-energy-cost"
  , shipBoostEnergyCost =
    shipBoostEnergyCost def
    &= explicit
    &= help "how much the energy regeneration slows down when using boost"
    &= name "ship-boost-energy-cost"
  , shipHealthRegen =
    shipHealthRegen def
    &= explicit
    &= help "the regeneration rate of a ship's health"
    &= name "ship-health-regen"
  , scanCost =
    scanCost def
    &= explicit
    &= help "the cost of a single scan"
    &= name "scan-cost"
  , turnCost =
    turnCost def
    &= explicit
    &= help "the cost of a single turn"
    &= name "turn-cost"
  , shootCost =
    shootCost def
    &= explicit
    &= help "the cost of a single shot"
    &= name "shoot-cost"
  }

runCmdArgs :: Config -> IO Config
runCmdArgs = cmdArgs . cmdConfFrom
