{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Input where

import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Bitraversable
import Data.Either as E
import Data.Map as M
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Vector as V hiding (foldl1, modify, sequence)
import Files
import Graphics
import Lonely
import Types
import Prelude hiding (Left, Right)

-- select input scheme from mode2
--  - read from file?
-- do input, recur
--  - find scheme?
-- paint with mode1

io :: IO a -> StateT Global IO a
io = liftIO

-- a full writeup is necessary. many iterations of this have come about

meat :: Char -> Image -> StateT Global IO (Image, Maybe Image)
meat input image = do
  global <- get
  let mode = global.mode
      env = (mode, image) -- aint i readable
      bleh = return ()
  extract $
    extend
      (grabScheme =>= selectCells input =>= colorCells)
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
  (Env, StateT Global IO ()) ->
  StateT Global IO Scheme
grabScheme shebang =
  let mode2 = snd . fst . fst $ shebang
      image = snd . fst $ shebang
   in return $ case mode2 of
        Stamp -> keySchemeStamp
        Text -> keySchemeText
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
  let image = snd . fst $ shebang
      scheme = snd shebang
   in do
        char2kc <- lookupU <$> (io $ do grabby) :: StateT Global IO (Char -> KeyCommand)
        scheme >>= (\x -> x image (char2kc input))

-- colorCells isn't gonna do any actual looping, its just gonna take a list of cells,
-- look at the current painting mode, and then return the image with those cells modified
colorCells ::
  ( Env,
    StateT
      Global
      IO
      (Either [RelCursor] (Image, Maybe Image))
  ) ->
  StateT Global IO (Image, Maybe Image)
colorCells shebang =
  let image = snd . fst $ shebang
      mode1 = fst . fst . fst $ shebang
      buncha = snd $ shebang
   in buncha >>= undefined

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

keySchemeText :: Scheme
keySchemeText = undefined

keySchemePoly :: Scheme
keySchemePoly = undefined

keySchemeLine :: Scheme
keySchemeLine image keycommand =
  let go x = do
        y <- keySchemeStamp image keycommand
        return $ validate [x, y]
   in iterateUntilM (E.either undefined undefined) go (Left [])

-- so heres the thing. the counit is supposed to be Either. but that sucks
class Bifunctor w => Bicomonad w where
  biextract :: Semigroup b => w a b -> (a, b)

  -- biextend lextract rextract = id
  -- biextend (fst . biextract) (snd . biextract) = id
  lextract :: w a b -> a
  rextract :: w a b -> b
  biduplicate :: w a b -> w (w a b) (w a b)

  -- biduplicate = biextend id id
  biextend :: (w a b -> c) -> (w a b -> d) -> w a b -> w c d

-- biextend f g = bimap f g . biduplicate

instance Bicomonad (,) where
  biextract = id
  lextract = fst
  rextract = snd
  biduplicate w = (w, w)
  biextend f g w = (f w, g w)
