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
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Map as M
import Data.Vector as V hiding (modify)

import System.IO
import System.Console.ANSI

import Types
import Graphics
import Files

io :: IO a -> StateT Global IO a
io = liftIO

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering -- see line 47
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT everything init_Global -- applicative .v.

everything :: StateT Global IO (Cofree Identity Image)
everything = unfoldM mainLoop (blankImage 30 30)

-- TODO: way of easily extending functionality with minimal reshuffling
-- possible fix:
--   type Between = (Image, (Int, Image -> StateT Global IO (Between, Identity Between))
--   mainLoop :: Between -> StateT Global IO (Between, Identity Between) 
--   maybe use map?
--
--   the least complicated solution is just to make Global bigger


mainLoop :: Image -> StateT Global IO (Image, Identity Image) 
mainLoop image = do
  global <- get
  io $ do clearScreen; setCursorPosition 0 0
  io . renderImage $ fullRender global image
  io $ hFlush stdout -- see line 34
  input <- io $ getChar
  modify $ inputHandler input image
  loadedImage <- io $ diskOp input image
  get >>= nextStep input loadedImage -- this saves having to repeat line 44 

nextStep :: Char -> Image -> Global -> StateT Global IO (Image, Identity Image)
nextStep input image global =
  if input == ' '
  then return (image, Identity $ update2d image global.relCursor (pack global))
  else return (image, Identity $ image)

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
               (M.adjust prev (stripI global.txSelect) (stripD global.txSelect))} 
    'd' -> global{relCursor = bimap (clampX . up  ) id global.relCursor}
    'a' -> global{relCursor = bimap (clampX . down) id global.relCursor}
    'w' -> global{relCursor = bimap id (clampY . down) global.relCursor}
    's' -> global{relCursor = bimap id (clampY . up  ) global.relCursor}
    'c' -> global{relCursor = bimap (clampX . up  ) (clampY . up  ) global.relCursor}
    'z' -> global{relCursor = bimap (clampX . down) (clampY . up  ) global.relCursor}
    'q' -> global{relCursor = bimap (clampX . down) (clampY . down) global.relCursor}
    'e' -> global{relCursor = bimap (clampX . up  ) (clampY . down) global.relCursor}
    _   -> global
  where down = (\x -> x - 1)
        up = (\x -> x + 1)
        clampX x = x `mod` (V.length image)
        clampY y = y `mod` (V.length $ V.head image)
-- this is so ugly
