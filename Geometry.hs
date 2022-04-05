{-# LANGUAGE DataKinds #-}

module Geometry where

import Data.Ext -- fuck off
import Data.Geometry.Point
import Data.Geometry.Polygon
import Types

cursorToPoints :: RelCursor -> Point 2 Int
cursorToPoints (x, y) = Point2 x y

cursorsToPolygon :: [RelCursor] -> SimplePolygon () Int
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
