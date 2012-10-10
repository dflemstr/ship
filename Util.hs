{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Util where

import Control.Concurrent.MVar

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.State.Lazy hiding (forM)

import Data.Functor

import Data.IxSet as IxSet

import Data.Lens.Common

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Traversable

import Data.Tuple

import Data.Typeable

showText :: Show a => a -> Text
showText = Text.pack . show

readText :: Read a => Text -> a
readText = read . Text.unpack

vecLen :: Double -> Double -> Double
vecLen x y = sqrt $ x * x + y * x

polarCoords :: Double -> Double -> (Double, Double)
polarCoords x y = (vecLen x y, atan2 y x)

data Circle =
  Circle
  { cx :: !Double, cy :: !Double
  , cr :: !Double
  }

data Line =
  Line
  { lx1 :: !Double, ly1 :: !Double
  , lx2 :: !Double, ly2 :: !Double
  }

{-# INLINABLE lineIntersects #-}
lineIntersects :: Line -> Circle -> Bool
lineIntersects (Line !x1 !y1 !x2 !y2) (Circle !x0 !y0 !r) =
  not $ dist > r || (d1 - r) > d || (d2 - r) > d
  where
    x21 = x2 - x1
    y21 = y2 - y1
    x01 = x0 - x1
    y01 = y0 - y1
    x02 = x0 - x2
    y02 = y0 - y2
    n = abs  $ x21 * (y1 - y0) - y21 * (x1 - x0)
    d = sqrt $ x21 * x21       + y21 * y21
    dist = n / d
    d1 = sqrt $ x01 * x01 + y01 * y01
    d2 = sqrt $ x02 * x02 + y02 * y02

mapIxState :: (Ord a, Typeable a, Indexable a,
               MonadState (IxSet a) m, Functor m)
           => StateT a m () -> m ()
mapIxState action = do
  elems <- IxSet.toList <$> get
  result <- forM elems $ execStateT action
  put $ IxSet.fromList result

alterMVar :: MonadIO m => MVar a -> StateT a IO b -> m b
alterMVar var action = liftIO . modifyMVar var $ fmap swap . runStateT action

justL :: Lens (Maybe a) a
justL = lens fromJust $ const . Just

access' :: MonadReader a m => Lens a b -> m b
access' = asks . getL
