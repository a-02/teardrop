{-# LANGUAGE BinaryLiterals #-}
-- bffs!
{-# LANGUAGE BlockArguments #-}
-- bffs!
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Char
import Data.Functor.Identity
import Data.Map as M
import Data.Vector as V hiding (foldl1, modify, sequence)
import Files
import Graphics
import Input
import System.Console.ANSI
import System.IO
import Types

-- whats this? its main!
-- first 3 things are standard IO bullshit, makin sure everything gets put where
-- it needs to go
-- clearScreen and setCursorPosition are obvious
-- runStateT... i'll explain
main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT everything initGlobal

-- what? alright this cofree thing solves a very problem
-- unfoldM's typesig is...
--
-- (Traversable f, Monad m) => (b -> m (a, f b)) -> b -> m (Cofree f a)
--
-- applying types...
--
--   (Image -> StateT Global IO (Image, Identity Image)) ->
--     Image -> StateT Global IO (Cofree Identity Image)
--
-- this sorta loop generates data, while keeping track of state
-- all wrapped around the IO monad
--
-- "but isn't `Cofree Identity a` just `Stream a` ?"
--
-- yes, it is. but it doesn't really have the same interface as the Cofree one.
--
-- eventually,  god willing, this program will grow in size and functionality
-- to necessitate modifying, restarting, swapping, or otherwise fiddling with
-- the structure of the loop itself.
--
-- the `f` in `Cofree f a` may soon be changed to a more complex type like
-- Maybe or Pair or whatever.
--
-- this is just `machines` but going the long way around, but. i argue. that
-- learning another package is less important than building lasting haskell understanding
everything :: StateT Global IO (Cofree Identity Image)
everything = unfoldM mainLoop (blankImage 30 30)

-- UNIMPL
mainLoop' :: Image -> StateT Global IO (Image, Maybe Image)
mainLoop' image = do
  initScript image
  inputScript image

-- UNIMPL
initScript :: Image -> StateT Global IO ()
initScript image = do
  global <- get
  io $ do clearScreen; setCursorPosition 0 0 -- top left
  io $ renderImage (fullRender global image)
  io $ hFlush stdout

-- UNIMPL
inputScript :: Image -> StateT Global IO (Image, Maybe Image)
inputScript image =
  let ifM b t f = do b <- b; if b then t else f
   in do
        input <- io $ getChar
        ifM
          (onModeInput input)
          ( do
              modeModify input
              return (image, Just image)
          )
          (paintModify input image)

-- UNIMPL
onModeInput :: Char -> StateT Global IO Bool
onModeInput input = return $ test input
  where
    test x = foldl1 (||) ((== x) <$> ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'])

-- UNIMPL
modeModify :: Char -> StateT Global IO ()
modeModify input = do
  modify $ \g -> g {mode = modeMatch g.mode input}
  where
    modeMatch mode@(m1, m2) input =
      case input of
        '1' -> (,) Normal m2
        '2' -> (,) ReplaceFGColor m2
        '3' -> (,) ReplaceBGColor m2
        '4' -> (,) ReplaceAllColor m2
        '5' -> (,) ReplaceText m2
        '6' -> (,) m1 Stamp
        '7' -> (,) m1 Text
        '8' -> (,) m1 Line
        '9' -> (,) m1 Polygon
        '0' -> (,) m1 PolyFill
        _ -> mode

-- UNIMPL; see Input.hs
paintModify :: Char -> Image -> StateT Global IO (Image, Maybe Image)
paintModify input image = meat input image
