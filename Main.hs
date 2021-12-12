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
  hSetBuffering stdout LineBuffering -- see line 48
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT meat init_Global -- applicative (:

meat :: StateT Global IO (Cofree Identity Image)
meat = unfoldM potatoes (blankImage 50 30)

potatoes :: Image -> StateT Global IO (Image, Identity Image)
potatoes image = do
  global <- get
  io $ do clearScreen; setCursorPosition 0 0
  io . renderImage $ fullRender global image
  io $ hFlush stdout -- see line 35
  input <- io $ getChar
  modify $ cheese input image
--  io $ saveNload input image
  get >>= kale input image -- this saves having to repeat line 45 


kale :: Char -> Image -> Global -> StateT Global IO (Image, Identity Image)
kale input image global =
  if input == ' '
  then return (image, Identity $ update2d image global.relCursor (pack global))
  else return (image, Identity $ image)

cheese :: Char -> Image -> Global -> Global
cheese input image global =
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
