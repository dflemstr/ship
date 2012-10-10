{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Prelude hiding ((.), id)

import Control.Applicative

import Control.Category

import Control.Concurrent hiding (yield)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.State.Class
import Control.Monad.State.Lazy hiding (mapState)

import Data.Aeson

import Data.Char

import Data.Conduit
import Data.Conduit.Network hiding (serverPort)
import qualified Data.Conduit.Text as Conduit

import Data.Default

import Data.Fixed

import Data.IxSet as IxSet

import Data.Lens
import Data.Lens.IxSet

import Data.Maybe

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Clock (DiffTime)

import Network.Socket

import System.Time.Monotonic

import Text.Shakespeare.Text

import CmdArgs
import Config
import Log
import Ship
import Util
import WebSocketServer

main :: IO ()
main = do
  conf <- runCmdArgs def
  mainWithConfig conf

mainWithConfig :: Config -> IO ()
mainWithConfig conf = do
  clock <- newClock
  initLog conf
  notice [st|starting server|]
  t <- clockGetTime clock
  c <- newMVar $ WsClients IxSet.empty 0
  s <- newMVar $ Ships IxSet.empty 0

  forkIO $ shipRegenerator conf s clock t
  info [st|spawned regeneration spark|]
  forkIO $ shipMover conf s clock t
  info [st|spawned movement spark|]
  forkIO $ runWebSocketServer (wsServerPort conf) c
  info [st|spawned websockets spark|]
  runTCPServer (tcpSettings conf) $ tcpApp conf s c

tcpSettings :: Monad m => Config -> ServerSettings m
tcpSettings conf = serverSettings (serverPort conf) HostAny

tcpApp :: Config -> MVar Ships -> MVar WsClients -> Application IO
tcpApp conf s c d = do
  notice $ [st|TCP client connected: #{show address}|]
  runPipe $
    injectLeftovers source >+>
    Conduit.decode Conduit.utf8 >+>
    Conduit.lines >+>
    injectLeftovers pipe >+>
    Conduit.encode Conduit.utf8 >+>
    injectLeftovers sink
  where
    pipe = appPipe conf s c address
    source = appSource d
    sink = appSink d
    address = appSockAddr d

appPipe :: Config -> MVar Ships -> MVar WsClients
        -> SockAddr -> Conduit Text IO Text
appPipe conf s c a = do
  joinMsg <- await
  case Text.words <$> joinMsg of
    Just ["join", uname', r, g, b] -> do
      let uname = Text.filter isPrint uname'
      notice [st|player joined: #{uname}|]
      let color = ShipColor (readText r) (readText g) (readText b)
      runAsUser conf s c uname color
    Just _ ->
      error' $ [st|disconnected player #{addr} |] <>
      [st|with indecipherable join message #{show (fromJust joinMsg)}|]
    _ ->
      warning
      [st|disconnected player #{addr} before receiving join message|]
  where
    addr = show a

data ShipRemoved = ShipRemoved ShipId

instance ToJSON ShipRemoved where
  toJSON (ShipRemoved sid) =
    object ["name" .= ("shipremoved" :: Text), "id" .= sid ]

nl :: Text
nl = "\n"

tellStatus :: Monad m => Ship -> Pipe l i Text u m ()
tellStatus s =
  yield [st|status #{health} #{energy}#{nl}|]
  where
    health = showText (floor $ getL shipHealth s :: Int)
    energy = showText (floor $ getL shipEnergy s :: Int)

tellPos :: Monad m => Config -> Ships -> Ship -> Pipe l i Text u m ()
tellPos conf ss s =
  forM_ (IxSet.toList . getL ships $ ss) . execStateT $ do
    sx <- access shipX
    sy <- access shipY
    name <- gets shipName
    let x = sx - myx
        y = sy - myy
    when (vecLen x y < range) $
      let (radius', angle') = polarCoords x y
          radius = showText $ (round radius' :: Int)
          angle = showText $ (round $ angle' * 180 / pi :: Int)
      in lift $ yield [st|pos #{name} #{radius} #{angle}#{nl}|]
  where
    myx = getL shipX s
    myy = getL shipY s
    range = shipScanRange conf

shoot :: Config -> MVar Ships -> Double -> Double -> Double -> IO ()
shoot conf s x y angle = do
  info [st|shooting from #{show x},#{show y} at #{show angle} degrees|]
  alterMVar s . focus ships . mapIxState $ do
    sx <- access shipX
    sy <- access shipY
    let hit = lineIntersects (Line x1 y1 x2 y2) (Circle sx sy r)
    shields <- access shipShields
    when hit . void $ do
      shipHealth -= dmg shields
      sid <- unShipId <$> gets shipId
      info [st|ship ##{show sid} was hit|]
      health <- access shipHealth
      tid <- gets shipThreadId
      when (health <= 0) . liftIO . killThread $ tid
  where
    dmg False = fromIntegral damage
    dmg True  = fromIntegral damageWithShields
    damage = shipDamage conf
    damageWithShields = shipDamageWithShields conf
    r = shipRadius conf
    range = shipShootRange conf
    dx = cos angle
    dy = sin angle
    x1 = x + (r + 1) * dx
    y1 = y + (r + 1) * dy
    x2 = x + range * dx
    y2 = y + range * dy

usingShip :: (MonadIO m, Functor m)
          => MVar Ships -> ShipId -> (Ship -> m a)-> m a
usingShip ms sid action = do
  s <- getL ships <$> liftIO (readMVar ms)
  case IxSet.getOne $ s @= sid of
    Just ship -> action ship
    Nothing -> do
      critical [st|lost ship with id ${unShipId sid} in read-only mode|]
      fail "Invalid ship id"

shipArray :: ShipId -> Text
shipArray sid = [st|ship[#{show (unShipId sid)}]|]

withShips :: MonadIO m => StateT Ships IO a -> MVar Ships -> m a
withShips action ms = alterMVar ms $ action

withShip :: MonadIO m => MVar Ships -> ShipId -> StateT Ship IO a -> m a
withShip ms sid action = flip withShips ms . focus (ixLens sid . ships) $ do
    m <- get
    case m of
      Just _ -> focus justL action
      Nothing -> do
        critical [st|lost ship with id ${unShipId sid} in read-write mode|]
        fail "Invalid ship id"

addUser :: (MonadIO m)
        => Config -> MVar Ships -> MVar WsClients
        -> Text -> ShipColor -> m ShipId
addUser conf s c uname color = do
  g <- liftIO getStdGen
  tid <- liftIO myThreadId
  flip evalRandT g $ do
    x <- getRandomR (0, fromIntegral width)
    y <- getRandomR (0, fromIntegral height)
    a <- getRandomR (0, 2 * pi)
    alterMVar s $ do
      sid <- shipsNextId += 1
      let ship =
            Ship sid tid uname color
            (ShipStatus
             (fromIntegral $ shipStartingEnergy conf)
             (fromIntegral $ shipStartingHealth conf))
            (ShipPosition x y a)
            (ShipState False False)
      focus ships . modify . IxSet.insert $ ship
      liftIO $ wsTellState c (shipArray sid) ship
      return sid
  where
    width = arenaWidth conf
    height = arenaHeight conf

removeUser :: MVar Ships -> MVar WsClients -> Text -> ShipId -> IO ()
removeUser s c uname sid = do
  notice [st|user #{uname}(#{show (unShipId sid)}) disconnected|]
  alterMVar s . focus ships . modify $ IxSet.deleteIx sid
  wsTellEvent c $ ShipRemoved sid

shipRegenerator :: Config -> MVar Ships
                -> Clock -> DiffTime -> IO ()
shipRegenerator conf s clock =
  loop
  where
    loop oldTime = do
      newTime <- clockGetTime clock
      let delta = realToFrac $ newTime - oldTime
      debug [st|regenerate update with frame time #{show delta}|]
      alterMVar s . focus ships .  mapIxState . void . regenShip $ delta
      threadDelay . round $ 1e6 / regenerationUpdateFrequency conf
      loop newTime
    regenShip dt = do
      boost <- access shipBoost
      shields <- access shipShields
      let energyInc =
            shipEnergyRegen conf
            - (if boost then shipBoostEnergyCost conf else 0)
            - (if shields then shipShieldEnergyCost conf else 0)
          hpInc = shipHealthRegen conf
      shipHealth += fromIntegral hpInc * dt
      shipHealth %= min (fromIntegral $ shipMaxHealth conf)
      shipHealth %= max 0
      shipEnergy += fromIntegral energyInc * dt
      shipEnergy %= min (fromIntegral $ shipMaxEnergy conf)
      shipEnergy %= max 0

shipMover :: Config -> MVar Ships
          -> Clock -> DiffTime -> IO ()
shipMover conf s clock = loop
  where
    loop oldTime = do
      newTime <- clockGetTime clock
      let delta = realToFrac $ newTime - oldTime
      debug [st|move update with frame time #{show delta}|]
      alterMVar s . focus ships . mapIxState . void . moveShip $ delta
      threadDelay . round $ 1e6 / movementUpdateFrequency conf
      loop newTime
    speed True = shipSpeedWithBoost conf
    speed False = shipSpeed conf
    wrapX = (`mod'` fromIntegral (arenaWidth conf))
    wrapY = (`mod'` fromIntegral (arenaHeight conf))
    moveShip delta = do
      heading <- access shipHeading
      boost <- access shipBoost
      let sp = speed boost
      shipX += cos heading * sp * delta
      shipX %= wrapX
      shipY += sin heading * sp * delta
      shipY %= wrapY

runAsUser :: Config -> MVar Ships -> MVar WsClients
          -> Text -> ShipColor -> Conduit Text IO Text
runAsUser conf s c uname color = do
  sid <- addUser conf s c uname color
  addCleanup (const $ removeUser s c uname sid) (runAsShipId conf s c uname sid)

consumeEnergy :: MonadIO m => MVar Ships -> ShipId -> Int -> m () -> m ()
consumeEnergy s sid i action = do
  should <- withShip s sid $ do
    energy <- access shipEnergy
    let success = energy >= di
    when success . void $ shipEnergy -= di
    return success
  when should action
  where
    di = fromIntegral i

wsTellShipL :: (ToJSON a, MonadIO m, Functor m)
            => MVar Ships -> MVar WsClients -> ShipId
            -> Lens Ship a -> Text -> m ()
wsTellShipL s c sid l n =
  usingShip s sid (return . getL l) >>=
  liftIO . wsTellState c (shipArray sid <> "." <> n)

runAsShipId :: Config -> MVar Ships -> MVar WsClients
            -> Text -> ShipId -> Conduit Text IO Text
runAsShipId conf s c uname sid =
  loop
  where
    loop = do
      message <- await
      health <- usingShip s sid $ return . getL shipHealth
      when (health > 0) $ continue message
    continue message = do
      let mwords = Text.words <$> message
      case message of
        Just msg ->
          let ssid = show . unShipId $ sid
          in notice [st|#{uname}(#{ssid}): #{show msg}|]
        Nothing -> return ()
      case mwords of
        Just ["scan"] -> do
          ss <- liftIO . readMVar $ s
          consumeEnergy s sid (scanCost conf) . usingShip s sid $
            tellPos conf ss
          wsTellShipL s c sid shipStatus "status"
          loop
        Just ["turn", angle] -> do
          consumeEnergy s sid (turnCost conf) $
            withShip s sid . void $ shipHeading += readText angle / 180 * pi
          wsTellShipL s c sid shipPosition "position"
          wsTellShipL s c sid shipStatus "status"
          loop
        Just ["shields", "on"] -> do
          withShip s sid $ shipShields != True
          wsTellShipL s c sid shipState "state"
          loop
        Just ["shields", "off"] -> do
          withShip s sid $ shipShields != False
          wsTellShipL s c sid shipState "state"
          loop
        Just ["boost", "on"] -> do
          withShip s sid $ shipBoost != True
          wsTellShipL s c sid shipState "state"
          loop
        Just ["boost", "off"] -> do
          withShip s sid $ shipBoost != False
          wsTellShipL s c sid shipState "state"
          loop
        Just ["shoot"] -> do
          consumeEnergy s sid 20 $
            usingShip s sid $ \ ship ->
            liftIO $ shoot conf s
            (getL shipX ship) (getL shipY ship) (getL shipHeading ship)
          loop
        Just ["status"] -> do
          usingShip s sid tellStatus
          loop
        _ -> return ()
