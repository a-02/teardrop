{-# LANGUAGE NegativeLiterals #-}

module Lonely where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Vector as V
import Types

clampX image x = x `mod` (V.length image)

clampY image y = y `mod` (V.length $ V.head image)

down :: Num a => a -> a
down = (+ -1)

up :: Num a => a -> a
up = (+ 1)

true = const True

whatever :: StateT Global IO (Either [a] b)
whatever = return $ Left []

io :: IO a -> StateT Global IO a
io = liftIO

setfst :: a -> (b, c) -> (a, c)
setfst x (a, b) = (x, b)

setsnd :: a -> (b, c) -> (b, a)
setsnd x (a, b) = (a, x)
