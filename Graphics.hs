{-# Language BinaryLiterals #-}
{-# Language OverloadedRecordDot #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Graphics where

import Types 

import Data.Char
import Data.Functor.Identity

import qualified Data.Vector as V
import qualified Data.Map.Strict as MS
import qualified Data.Map.Lazy as M

import System.Console.ANSI

import Control.Monad.Trans.State.Lazy

latin1, latinSupp, latinExtA, box, block, braille :: [Char]
latin1    = chr <$> [0x0020..0x007e]
latinSupp = chr <$> [0x00a1..0x00ac] ++ [0x00ae..0x00ff]
latinExtA = chr <$> [0x0100..0x017f]
box       = chr <$> [0x2500..0x257f]
block     = chr <$> [0x2580..0x259f]
braille   = chr <$> [0x2800..0x28ff]

pageMatch :: Page -> [Char]
pageMatch = \case
  Latin1    -> latin1
  LatinSupp -> latinSupp
  LatinExtA -> latinExtA
  Box       -> box
  Block     -> block
  Braille   -> braille

intMap xs = MS.fromList $ zip [1..] xs --for my next project i SWEAR ill keep things indexed to 0 

allUnicodeMaps :: [Statelike Int Char]
allUnicodeMaps = lift' . intMap <$> [latin1, latinSupp, latinExtA, box, block, braille]

init_txSelect :: Statelike Page (Statelike Int Char)
init_txSelect = lift' . MS.fromList $ zip [minBound..maxBound] allUnicodeMaps

init_fgSelect :: Statelike Int FGColor
init_fgSelect = lift' . MS.fromList $ zip [0..] allFGColor

init_bgSelect :: Statelike Int BGColor
init_bgSelect = lift' . MS.fromList $ zip [0..] allBGColor

init_Global :: Global
init_Global = 
  Global init_txSelect 
         init_fgSelect 
         init_bgSelect
         (0, 0)
         (Normal, Stamp)

allFGColor :: [FGColor]
allFGColor = ((FGColor Dull) <$> [minBound..maxBound]) ++ ((FGColor Vivid) <$> [minBound..maxBound])

allBGColor :: [BGColor]
allBGColor = ((BGColor Dull) <$> [minBound..maxBound]) ++ ((BGColor Vivid) <$> [minBound..maxBound])

defaultColor = toSGR $ FullColor (FGColor Vivid White) (BGColor Dull Black)

emptyCell = (defaultColor, ' ')

testCell = (toSGR $ FGColor Vivid Red, '5')

blankImage :: Int -> Int -> Image
blankImage x y = V.replicate y (V.replicate x emptyCell) 

testImage :: Int -> Int -> Image
testImage x y = V.replicate y (V.replicate x testCell)

renderImage :: Image -> IO ()
renderImage image = do
  clearScreen
  setCursorPosition 0 0 
  sequence_ $ renderRow <$> image
  setSGR [Reset]

(<->) :: Image -> Image -> Image -- append two images horizontally, provided b is shorter than a
a <-> b = (V.zipWith (<>) a b) V.++ (V.drop (V.length b) a)

imagePlusUI :: Global -> Image -> Image
imagePlusUI global image = (renderCursor (relCursor global) image) <-> (uiFromGlobal global)

fullRender = imagePlusUI

uiFromGlobal :: Global -> Image
uiFromGlobal global = V.fromList $
  [ colorUITop 
  , colorUIBottom (stripI $ global.fgSelect)
  , colorUITop
  , colorUIBottom (stripI $ global.bgSelect)
  ] ++ (pageUI global.txSelect)
 
chunksOf :: Eq a => Int -> [a] -> [[a]] 
chunksOf a xs = 
  let (ys,zs) = (take a xs, drop a xs) 
  in  if ys == [] 
      then [] 
      else ys : (chunksOf a zs)

pageUI :: Statelike Page (Statelike Int Char) -> [V.Vector Cell]
pageUI tx = 
  let inner     = (stripD tx) MS.! (stripI tx)
      formatted = (defaultColor, ) <$> (M.elems $ stripD inner) :: [Cell]
      colorized = 
        let (front, back) = splitAt (stripI inner) formatted
            selected x   = (\x (a,b) -> (,) x b) cursorColor x :[] 
         in if front == []
            then selected (head back) ++ (tail back)
            else init front ++ selected (last front) ++ back
   in V.fromList <$> chunksOf 16 colorized
   -- to myself: rewrite this

colorUITop :: V.Vector Cell
colorUITop = V.fromList $ zip (toSGR <$> allBGColor) (replicate 16 ' ')

colorUIBottom :: Int -> V.Vector Cell
colorUIBottom index = 
  let blankRow = V.fromList $ zip (replicate 16 defaultColor) (replicate 16 ' ')
      selector = (defaultColor, '^')
   in if   index <= 16 && index >= 0
      then update' blankRow [(index, selector)]
      else update' blankRow [(5, selector)] -- this, most likely, will not occur

renderCursor :: RelCursor -> Image -> Image 
renderCursor rc@(r,c) image = 
  let x = fromIntegral r
      y = fromIntegral c
      xy = (x, y)
      updatedX = (,) x (cursorColor, snd $ image !!! xy)
      updatedY = (,) y (update' (image V.! y) [updatedX]) 
   in update' image [updatedY]

(!!!) :: V.Vector (V.Vector a) -> (Int, Int) -> a
a !!! (x,y) = (a V.! y) V.! x

update' :: V.Vector a -> [(Int, a)] -> V.Vector a
update' x y = x V.// y

update2d :: V.Vector (V.Vector a) -> (Int,Int) -> a -> V.Vector (V.Vector a)
update2d grid (row,column) item = 
  let inner = update' (grid V.! column) [(row, item)]
   in update' grid [(column, inner)] -- ask me no questions, and i will tell you no questions
      
cursorColor :: [SGR]
cursorColor = toSGR $ FullColor (FGColor Vivid Red) (BGColor Vivid Blue )

renderRow :: V.Vector Cell -> IO ()
renderRow row = (sequence_ $ renderCell <$> row) >> (cursorDownLine 1)

renderCell :: Cell -> IO ()
renderCell cell = (setSGR $ fst cell) >> (putChar $ snd cell) >> (setSGR [Reset])
