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
import Lonely
import System.Console.ANSI
import System.IO
import Types

{-
 main sets all the terminal options so that the program runs smoothly,
 and then runs the drawing loop.
-}

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  clearScreen
  setCursorPosition 0 0
  pure () <* runStateT everything initGlobal

{-
 what? alright this cofree thing solves a very problem
 unfoldM's typesig is...

 (Traversable f, Monad m) => (b -> m (a, f b)) -> b -> m (Cofree f a)

 applying types...

   (Image -> StateT Global IO (Image, Identity Image)) ->
     Image -> StateT Global IO (Cofree Identity Image)

 this sorta loop generates data, while keeping track of state
 all wrapped around the IO monad

 "but isn't `Cofree Identity a` just `Stream a` ?"

 yes, it is. but it doesn't really have the same interface as the Cofree one.

 eventually,  god willing, this program will grow in size and functionality
 to necessitate modifying, restarting, swapping, or otherwise fiddling with
 the structure of the loop itself.

 the `f` in `Cofree f a` may soon be changed to a more complex type like
 Maybe or Pair or whatever.

 note: this is also, to my knowledge what `machines` and `pipes` do, but the comonad-ness
 is hidden from view.
-}

everything :: StateT Global IO (Cofree (Iddy [RelCursor]) Image)
everything = unfoldM mainLoop (blankImage 30 30)

mainLoop :: Image -> StateT Global IO ImagePack
mainLoop image = do
  initScript image
  inputScript image

initScript :: Image -> StateT Global IO ()
initScript image = do
  global <- get
  io $ do clearScreen; setCursorPosition 0 0 -- top left
  io $ renderImage (fullRender global image)
  io $ hFlush stdout

inputScript :: Image -> StateT Global IO ImagePack
inputScript image =
  let ifM b t f = do b <- b; if b then t else f
   in do
        input <- io $ getChar
        ifM
          (onModeInput input)
          ( do
              modeModify input
              return (image, Continue image)
          )
          (paintModify input image)

onModeInput :: Char -> StateT Global IO Bool
onModeInput input = return $ test input
  where
    test x = foldl1 (||) ((== x) <$> ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'])

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
