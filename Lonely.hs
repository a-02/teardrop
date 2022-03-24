module Lonely where

import Control.Monad.Trans.State.Lazy
import Data.Vector as V
import Types

clampX image x = x `mod` (V.length image)

clampY image y = y `mod` (V.length $ V.head image)

down = (\x -> x - 1)

up = (\x -> x + 1)

whatever :: StateT Global IO (Either [a] b)
whatever = return $ Left []
