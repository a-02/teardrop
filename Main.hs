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
import Data.Map as M
import Data.Vector as V hiding (modify)

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
  hSetBuffering stdout LineBuffering
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT meat init_Global -- applicative (:

meat :: StateT Global IO (Cofree Identity Image)
meat = unfoldM potatoes (testImage 30 30)

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
  io $ hFlush stdout
  input <- io $ getChar
  modify $ inputHandler input image
  return (image, Identity $ image)


inputHandler :: Char -> Image -> Global -> Global
inputHandler input image global =
  case input of
    'i' -> global{fgSelect = prev global.fgSelect} 
    'o' -> global{fgSelect = next global.fgSelect} 
    'k' -> global{bgSelect = prev global.bgSelect}
    'l' -> global{bgSelect = next global.bgSelect}
    'y' -> global{txSelect = prev global.txSelect}
    'u' -> global{txSelect = next global.txSelect}
    'j' -> global{txSelect = 
             Statelike 
               (stripI global.txSelect) 
               (M.adjust next (stripI global.txSelect) (stripD global.txSelect))}
    'h' -> global{txSelect = 
             Statelike
               (stripI global.txSelect) 
               (M.adjust prev (stripI global.txSelect) (stripD global.txSelect))} -- there has got to be a better way
    'd' -> global{relCursor = bimap (clamp . up) id global.relCursor}
    'a' -> global{relCursor = bimap (clamp . down) id global.relCursor}
    'w' -> global{relCursor = bimap id (clamp . down) global.relCursor}
    's' -> global{relCursor = bimap id (clamp . up) global.relCursor}
    'c' -> global{relCursor = bimap (clamp . up) (clamp . up) global.relCursor}
    'z' -> global{relCursor = bimap (clamp . down) (clamp . up) global.relCursor}
    'q' -> global{relCursor = bimap (clamp . down) (clamp . down) global.relCursor}
    'e' -> global{relCursor = bimap (clamp . up) (clamp . down) global.relCursor}
    _   -> global
  where down = (\x -> x - 1)
        up = (\x -> x + 1)
        clamp x = x `mod` (V.length image)
{--
uh, UX stuff
WASD: move around the canvas
space: draw
HJ: fgcolor select
KL: bgcolor select
YU: page select
IO: index select
--}
