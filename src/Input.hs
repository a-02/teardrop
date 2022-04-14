{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Input where

import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Either as E
import Data.IORef -- see comment at bottom of file
import Data.List
import Data.Map as M
import Data.Vector as V hiding (foldl1, modify, sequence)
import Data.Void
import Files
import Geometry
import Graphics
import Lens.Micro
import Lonely
import Types
import Prelude as P

-- a full writeup is necessary. many iterations of this have come about

meat :: Char -> Image -> StateT Global IO (Image, Maybe Image)
meat input image = do
  global <- get
  let mode = global.mode
      env = (mode, image) -- aint i readable
      bleh = return ()
  extract $
    extend
      (grabScheme input =>= selectCells input =>= colorCells input)
      (env, bleh)

-- extract :: (e, a) -> a
-- extend :: ( (e, a) -> b ) -> (e, a) -> (e, b)
-- (=>=) :: ( (e, a) -> b ) -> ( (e, b) -> c ) -> (e, a) -> c

{-
type Scheme =
  Image ->
  KeyCommand ->
  StateT Global IO
    (Either
      [RelCursor]
      (Image, Maybe Image))
-}

grabScheme ::
  Char ->
  (Env, StateT Global IO ()) ->
  StateT Global IO Scheme
grabScheme input shebang =
  let mode2 = shebang ^. _1 . _1 . _2
      image = shebang ^. _1 . _2
   in return $ case mode2 of
        Stamp -> keySchemeStamp
        Text -> keySchemeText input
        Line -> keySchemeLine
        Polygon -> keySchemePoly
        PolyFill -> keySchemePoly

-- selectCells is a weird bit of half-accidental design.
-- i dont really want users to be able to change modes in the middle of drawing with the mode
-- so this sorta locks you into it until you finish doin inputs.
-- yknow. like mspaint.
selectCells ::
  Char ->
  (Env, StateT Global IO Scheme) -> -- mode, original image, and ANOTHER STATE MONAD
  ( StateT
      Global
      IO
      ( Either
          [RelCursor]
          (Image, Maybe Image)
      )
  )
selectCells input shebang =
  let image = shebang ^. _1 . _2
      scheme = shebang ^. _2
   in do
        char2kc <- io $ readIORef =<< char2kcRef' :: StateT Global IO (Char -> KeyCommand)
        scheme >>= (\x -> x image (char2kc input))

-- colorCells isn't gonna do any actual looping, its just gonna take a list of cells,
-- look at the current painting mode, and then return the image with those cells modified
colorCells ::
  Char ->
  ( Env,
    StateT
      Global
      IO
      (Either [RelCursor] (Image, Maybe Image))
  ) ->
  StateT Global IO (Image, Maybe Image)
colorCells input shebang =
  let image = shebang ^. _1 . _2
      mode = shebang ^. _1 . _1
      schemeResult = shebang ^. _2
   in schemeResult >>= painty image mode

painty ::
  Image ->
  Mode ->
  Either [RelCursor] (Image, Maybe Image) ->
  StateT Global IO (Image, Maybe Image)
painty image (mode1, mode2) = \case
  Right x -> return x
  Left cursors ->
    let line = (\(x : y : nothing) -> bresenhams x y) cursors
        poly = pixelsOnBoundary cursors
        polyfill = pixelsInPolygon cursors
        update global (x :: RelCursor) = update2d image x ((mode1ToPack mode1) global) :: Image
        paintMeat g xs = (Just . foldl1 magma) $ update g <$> xs
     in do
          g <- get -- i never thought i could do this
          case mode2 of
            Stamp -> return (image, paintMeat g cursors) -- im stupid
            Text -> return $ error "how did this happen?"
            Line -> return (image, paintMeat g line)
            Polygon -> return (image, paintMeat g poly)
            PolyFill -> return (image, paintMeat g polyfill)

-- cheating and just updating the image now
keySchemeText :: Char -> Scheme
keySchemeText input image keycommand =
  do
    g <- get
    let cursor = g.relCursor
        mode1 = fst g.mode
        cy = clampY image
    modify \g -> g {relCursor = bimap id (cy . up) cursor}
    return $ Right $ (,) image (Just $ update2d image cursor (setsnd input $ mode1ToPack mode1 g))

-- explainer:
--
-- a Scheme is a function taking a KeyCommand and an Image
-- and produces a stateful action, more often than not moving the cursor
--
-- but. what i've also rolled into it is this also either being a Left [RelCursor]
-- with which colorCells will read
--
-- it should be stated. under normal usage, `Select` will be what triggers painting to occur
keySchemeStamp :: Scheme
keySchemeStamp image keycommand =
  let clampX x = x `mod` (V.length image)
      clampY y = y `mod` (V.length $ V.head image)
      down = (\x -> x - 1)
      up = (\x -> x + 1)
   in case keycommand of
        Up -> do modify \g -> g {relCursor = bimap id (clampY . down) g.relCursor}; whatever
        Down -> do modify \g -> g {relCursor = bimap id (clampY . up) g.relCursor}; whatever
        KLeft -> do modify \g -> g {relCursor = bimap (clampX . down) id g.relCursor}; whatever
        KRight -> do modify \g -> g {relCursor = bimap (clampX . up) id g.relCursor}; whatever
        UpLeft -> do modify \g -> g {relCursor = bimap (clampX . down) (clampY . down) g.relCursor}; whatever
        UpRight -> do modify \g -> g {relCursor = bimap (clampX . up) (clampY . down) g.relCursor}; whatever
        DownLeft -> do modify \g -> g {relCursor = bimap (clampX . down) (clampY . up) g.relCursor}; whatever
        DownRight -> do modify \g -> g {relCursor = bimap (clampX . up) (clampY . up) g.relCursor}; whatever
        PrevFG -> do modify \g -> g {fgSelect = prev g.fgSelect}; whatever
        NextFG -> do modify \g -> g {fgSelect = next g.fgSelect}; whatever
        PrevBG -> do modify \g -> g {bgSelect = prev g.bgSelect}; whatever
        NextBG -> do modify \g -> g {bgSelect = next g.bgSelect}; whatever
        PrevPG -> do modify \g -> g {txSelect = prev g.txSelect}; whatever
        NextPG -> do modify \g -> g {txSelect = next g.txSelect}; whatever
        PrevTX ->
          do
            modify \g ->
              g
                { txSelect =
                    Statelike
                      (stripI g.txSelect)
                      (M.adjust prev (stripI g.txSelect) (stripD g.txSelect))
                }
            whatever
        NextTX ->
          do
            modify \g ->
              g
                { txSelect =
                    Statelike
                      (stripI g.txSelect)
                      (M.adjust next (stripI g.txSelect) (stripD g.txSelect))
                }
            whatever
        Save ->
          do io $ saveImage image; whatever
        Load ->
          do
            what <- io $ loadImage
            return $ Right (what, Just what)
        Select ->
          do
            g <- get
            let rel = g.relCursor
            return $ Left [rel]
        Dummy -> whatever

keySchemePoly :: Scheme
keySchemePoly image keycommand =
  let go x = do
        input <- io $ getChar
        c2kc <- io $ readIORef =<< char2kcRef' -- huh?
        y <- keySchemeStamp image (c2kc input)
        return $ validate [x, y]
   in keySchemeStamp image keycommand
        >>= iterateUntilM
          ( E.either
              polyTest
              true
          )
          go

polyTest x =
  let (a : b : rest) = P.reverse x
   in a == b

keySchemeLine :: Scheme
keySchemeLine image keycommand =
  let go x = do
        input <- io $ getChar
        c2kc <- io $ readIORef =<< char2kcRef'
        y <- keySchemeStamp image (c2kc input)
        return $ validate [x, y]
   in keySchemeStamp image keycommand
        >>= iterateUntilM
          ( E.either
              (\x -> P.length x == 2)
              true
          )
          go

{- on IORef;

its actually ok to use ioref. really if you just use one and you dont modify it, it cant hurt you.
here, it is being used to keep a file in memory and not have to read and parse it every time
an input is read.

the configuration function is kept in RAM (i believe, not sure where else it could be), makes it
a lot easier than keeping the whole bytestring and then parsing it every time

-}
