{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geometry where

import Data.Ext -- fuck off
import Data.Geometry.Point
import Data.Geometry.Polygon
import Types

cursorToPoints :: RelCursor -> Point 2 Float
cursorToPoints (x, y) = Point2 (fromIntegral x) (fromIntegral y)

pointsToCursor :: Point 2 Float -> RelCursor
pointsToCursor (Point2 x y) = (fromEnum x, fromEnum y)

cursorsToPolygon :: [RelCursor] -> SimplePolygon () Float
cursorsToPolygon xs = fromPoints $ (ext . cursorToPoints) <$> xs

boundingBox :: [RelCursor] -> [RelCursor]
boundingBox xs =
  let x = fst <$> xs
      y = snd <$> xs
   in [(i, j) | i <- [minimum x, maximum x], j <- [minimum y, maximum y]]

pointsinBoundingBox :: [RelCursor] -> [RelCursor]
pointsinBoundingBox xs =
  let x = fst <$> xs
      y = snd <$> xs
   in [(i, j) | i <- [minimum x .. maximum x], j <- [minimum y .. maximum y]]

pixelsOnBoundary :: [RelCursor] -> [RelCursor]
pixelsOnBoundary xs =
  let poly = cursorsToPolygon xs
      checkingPoints = cursorToPoints <$> pointsinBoundingBox (boundingBox xs)
   in fmap pointsToCursor $ filter (\x -> x `onBoundary` poly) checkingPoints

pixelsInPolygon :: [RelCursor] -> [RelCursor]
pixelsInPolygon xs =
  let poly = cursorsToPolygon xs
      checkingPoints = cursorToPoints <$> pointsinBoundingBox (boundingBox xs)
   in fmap pointsToCursor $ filter (\x -> x `onBoundary` poly || x `insidePolygon` poly) checkingPoints
