{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Graphics where

import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.Functor.Identity
import qualified Data.Map.Lazy as M
import qualified Data.Map.Strict as MS
import qualified Data.Vector as V
import Data.Void
import System.Console.ANSI
import Types

-- Corresponding to the codepoints of every legal character in the Spleen font.
latin1, latinSupp, latinExtA, box, block, braille :: [Char]
latin1 = chr <$> [0x0020 .. 0x007e]
latinSupp = chr <$> [0x00a1 .. 0x00ac] ++ [0x00ae .. 0x00ff]
latinExtA = chr <$> [0x0100 .. 0x017f]
box = chr <$> [0x2500 .. 0x257f]
block = chr <$> [0x2580 .. 0x259f]
braille = chr <$> [0x2800 .. 0x28ff]

-- Complete mapping of Pages to [Chars]
pageMatch :: Page -> [Char]
pageMatch = \case
  Latin1 -> latin1
  LatinSupp -> latinSupp
  LatinExtA -> latinExtA
  Box -> box
  Block -> block
  Braille -> braille

intMap xs = MS.fromList $ zip [1 ..] xs -- for my next project i SWEAR ill keep things indexed to 0

allUnicodeMaps :: [Statelike Int Char]
allUnicodeMaps = lift' . intMap <$> [latin1, latinSupp, latinExtA, box, block, braille]

initTXSelect :: Statelike Page (Statelike Int Char)
initTXSelect = lift' . MS.fromList $ zip [minBound .. maxBound] allUnicodeMaps

initFGSelect :: Statelike Int FGColor
initFGSelect = lift' . MS.fromList $ zip [0 ..] allFGColor

initBGSelect :: Statelike Int BGColor
initBGSelect = lift' . MS.fromList $ zip [0 ..] allBGColor

initGlobal :: Global
initGlobal =
  Global
    initTXSelect
    initFGSelect
    initBGSelect
    (0, 0)
    (Normal, Stamp)

allFGColor :: [FGColor]
allFGColor = ((FGColor Dull) <$> [minBound .. maxBound]) ++ ((FGColor Vivid) <$> [minBound .. maxBound])

allBGColor :: [BGColor]
allBGColor = ((BGColor Dull) <$> [minBound .. maxBound]) ++ ((BGColor Vivid) <$> [minBound .. maxBound])

defaultColor = toSGR $ FullColor (FGColor Vivid White) (BGColor Dull Black)

-- An emptyCell doesn't contain \NUL, due to this type's Unital instance.
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

-- this could be better. this leaves a huge fuckin GAP at the bottom right
(<->) :: Image -> Image -> Image
a <-> b = (V.zipWith (<>) a b) V.++ (V.drop (V.length b) a)

imagePlusUI :: Global -> Image -> Image
imagePlusUI global image = (renderCursor (relCursor global) image) <-> (uiFromGlobal global)

-- readability definition
fullRender = imagePlusUI

-- is this not self-documenting? colorUITop is just the colors, bottom is the arrow showing
-- what its pointing to. pageUI shows the texto things
uiFromGlobal :: Global -> Image
uiFromGlobal global =
  V.fromList $
    [ colorUITop,
      colorUIBottom (stripI $ global.fgSelect),
      colorUITop,
      colorUIBottom (stripI $ global.bgSelect)
    ]
      ++ (pageUI global.txSelect)

-- i know theres a foldr way to do this. but this is readable too
chunksOf :: Eq a => Int -> [a] -> [[a]]
chunksOf a xs =
  let (ys, zs) = (take a xs, drop a xs)
   in if ys == []
        then []
        else ys : (chunksOf a zs)

-- what the fuck?
pageUI :: Statelike Page (Statelike Int Char) -> [V.Vector Cell]
pageUI tx =
  let inner = (stripD tx) MS.! (stripI tx)
      formatted = (defaultColor,) <$> (M.elems $ stripD inner) :: [Cell]
      colorized =
        let (front, back) = splitAt (stripI inner) formatted
            selected x = (\x (a, b) -> (,) x b) cursorColor x : []
         in if front == []
              then selected (head back) ++ (tail back)
              else init front ++ selected (last front) ++ back
   in V.fromList <$> chunksOf 16 colorized

-- a row of all the colors, not a full image.
colorUITop :: V.Vector Cell
colorUITop = V.fromList $ zip (toSGR <$> allBGColor) (replicate 16 ' ')

-- this just puts a ^ character at a certain position in a blank row
-- the else-case will not happen, but it hasn't been tested.
-- so as it stands, snapping to the middle should be enough of an indication
-- that something has gone wrong with my math
colorUIBottom :: Int -> V.Vector Cell
colorUIBottom index =
  let blankRow = V.fromList $ zip (replicate 16 defaultColor) (replicate 16 ' ')
      selector = (defaultColor, '^')
   in if index <= 16 && index >= 0
        then update' blankRow [(index, selector)]
        else update' blankRow [(8, selector)]

renderCursor :: RelCursor -> Image -> Image
renderCursor rc@(r, c) image =
  let x = fromIntegral r
      y = fromIntegral c
      xy = (x, y) -- i'm sorry. parentheses are ugly
      updatedX = (,) x (cursorColor, snd $ index2dX image xy)
      updatedY = (,) y (update' (image V.! y) [updatedX])
   in update' image [updatedY]

-- index given is done row first, then column.
index2dX :: V.Vector (V.Vector a) -> (Int, Int) -> a
index2dX a (x, y) = (a V.! y) V.! x

-- convenience prefix notation?
update' :: V.Vector a -> [(Int, a)] -> V.Vector a
update' x y = x V.// y

update2d :: V.Vector (V.Vector a) -> (Int, Int) -> a -> V.Vector (V.Vector a)
update2d grid (row, column) item =
  let inner = update' (grid V.! column) [(row, item)]
   in update' grid [(column, inner)] -- ask me no questions, and i will tell you no questions

-- exposing this to the user would require it to always be in scope during the entire IO stack
-- aka, the entire program runtime.
-- if someone using this wants to change the cursor color, they have to change this definition
-- and then recompile.
--
-- maybe theres a way to read this sorta stuff from a file,
cursorColor :: [SGR]
cursorColor = toSGR $ FullColor (FGColor Vivid Red) (BGColor Vivid Blue)

-- sometimes you just wanna use applicative
renderRow :: V.Vector Cell -> IO ()
renderRow row = (sequence_ $ renderCell <$> row) *> (cursorDownLine 1)

-- at the very core of everything is a lot of very specific putChar's.
-- its uncertain if the last part of this is useful, but things seem to be working
-- as intended. more research required here.
renderCell :: Cell -> IO ()
renderCell cell = (setSGR $ fst cell) *> (putChar $ snd cell) *> (setSGR [Reset])

-- i dont know how this works. this was "adapted" from LambdaHack
-- to myself: actually make a readable bresenhams in haskell

bresenhams :: RelCursor -> RelCursor -> [RelCursor]
bresenhams (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx, y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step)
        | abs dx > abs dy = (abs dy, abs dx, xyStep)
        | otherwise = (abs dx, abs dy, yxStep)
      bw = balancedWord p q
      walk w xy = xy : walk (tail w) (step (head w) xy)
   in walk bw (x0, y0)

balancedWord :: Int -> Int -> [Int]
balancedWord p q | p < q = 0 : balancedWord p q
balancedWord p q = 1 : balancedWord p q
