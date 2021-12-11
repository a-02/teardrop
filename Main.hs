{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Char
import Data.Functor.Identity

import Control.Comonad.Cofree
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Control.Comonad
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Bifunctor.Flip

import System.IO
import System.Console.ANSI

import Types
import Graphics

io :: IO a -> StateT Global IO a
io = liftIO

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT meat init_Global -- applicative (:

meat :: StateT Global IO (Cofree Identity Image)
meat = unfoldM potatoes (blankImage 30 30)

-- potato flow
-- render the screen
-- grab input
-- do thing with input
-- back to start

potatoes :: Image -> StateT Global IO (Image, Identity Image)
potatoes image = do
  global <- get
  io $ do clearScreen; setCursorPosition 0 0
  io . renderImage $ fullRender global image
  input <- io $ getChar
  modify . check $ inputHandler input
  return (image, Identity $ image)

check = id -- bounds checking!!!!!

inputHandler :: Char -> Global -> Global
inputHandler input global =
  case input of
    'i' -> global{fgSelect = prev global.fgSelect} 
    'o' -> global{fgSelect = next global.fgSelect} 
    'k' -> global{bgSelect = prev global.bgSelect}
    'l' -> global{bgSelect = next global.bgSelect}
    'y' -> global{txSelect = prev global.txSelect}
    'u' -> global{txSelect = next global.txSelect}
    'h' -> undefined -- prev index
    'j' -> undefined -- next index
    'w' -> global{relCursor = bimap up id global.relCursor}
    'a' -> global{relCursor = bimap down id global.relCursor}
    's' -> global{relCursor = bimap id down global.relCursor}
    'd' -> global{relCursor = bimap id up global.relCursor}
    _   -> undefined
  where down = (\x -> x - 1)
        up = (\x -> x + 1)
{--
uh, UX stuff
WASD: move around the canvas
space: draw
HJ: fgcolor select
KL: bgcolor select
YU: page select
IO: index select
--}
