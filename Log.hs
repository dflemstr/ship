module Log where

import Prelude hiding (log)

import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI

import System.IO

import System.Log.Formatter
import System.Log.Handler as Handler
import System.Log.Handler.Simple hiding (priority)
import System.Log.Logger as Logger

import Config

initLog :: Config -> IO ()
initLog conf = do
  s' <- streamHandler stderr p
  let s = s' `setFormatter` termFormatter
  f <- fileHandler "ship.log" p
  updateGlobalLogger l (Logger.setLevel p)
  updateGlobalLogger l (setHandlers [s, f])
  where
    l = rootLoggerName
    p = logPriority conf

termFormatter :: LogFormatter a
termFormatter _ (prio, msg) _ =
  return $ termPrioString prio ++ " " ++ msg ++ setSGRCode [Reset]

termPrioString :: Priority -> String
termPrioString p =
  setSGRCode [SetColor Foreground Vivid color] ++ "[" ++
  setSGRCode [SetColor Foreground Dull color] ++ show p ++
  setSGRCode [SetColor Foreground Vivid color] ++ "]" ++
  setSGRCode [SetColor Foreground Dull color]
  where
    color = termPrioColor p

termPrioColor :: Priority -> Color
termPrioColor DEBUG = Black
termPrioColor INFO = White
termPrioColor NOTICE = Green
termPrioColor WARNING = Yellow
termPrioColor ERROR = Red
termPrioColor CRITICAL = Magenta
termPrioColor ALERT = Cyan
termPrioColor EMERGENCY = Blue

log :: MonadIO m => Priority -> Text -> m ()
log p = liftIO . logM "ship" p . Text.unpack

debug :: MonadIO m => Text -> m ()
debug = log DEBUG

info :: MonadIO m => Text -> m ()
info = log INFO

notice :: MonadIO m => Text -> m ()
notice = log NOTICE

warning :: MonadIO m => Text -> m ()
warning = log WARNING

error' :: MonadIO m => Text -> m ()
error' = log ERROR

critical :: MonadIO m => Text -> m ()
critical = log CRITICAL

alert :: MonadIO m => Text -> m ()
alert = log ALERT

emergency :: MonadIO m => Text -> m ()
emergency = log EMERGENCY
