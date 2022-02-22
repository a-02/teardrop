{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Types where

import           System.Console.ANSI
import           Data.Serialize as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import           Control.Comonad
import           Data.Char
import           Data.Monoid
import           Control.Monad.Trans.State.Lazy

-- STATELIKE, GLOBAL, AND ITS FRIENDS

data Statelike s a = Statelike s (M.Map s a) deriving (Eq, Show) 

data Global = Global
  { txSelect  :: Statelike Page (Statelike Int Char) -- what character?
  , fgSelect  :: Statelike Int FGColor -- what color is the character?
  , bgSelect  :: Statelike Int BGColor -- what color is the background?
  , relCursor :: RelCursor -- where is the cursor on the canvas?
  , mode      :: (Mode1, Mode2) -- what brush am i using?
  } deriving (Eq, Show)

collapse :: Ord s => Statelike s a -> a
collapse sl = stripD sl M.! stripI sl

packAll :: Global -> Cell
packAll global = 
  (,) (toSGR $ FullColor (collapse global.fgSelect) (collapse global.bgSelect)) 
      (collapse . collapse $ global.txSelect) -- coolest line of code ive ever written

packColor :: Global -> Cell
packColor global = 
  (,) (toSGR $ FullColor (collapse global.fgSelect) (collapse global.bgSelect)) '\NUL'

packFG :: Global -> Cell
packFG global = (toSGR $ collapse global.fgSelect, '\NUL')

packBG :: Global -> Cell
packBG global = (toSGR $ collapse global.bgSelect, '\NUL')

packText :: Global -> Cell
packText global = ( [], collapse . collapse $ global.txSelect )

stripIndexFromStatelike :: Statelike s a -> s
stripIndexFromStatelike (Statelike s a) = s -- remembering const functor fondly

stripDataFromStatelike :: Statelike s a -> M.Map s a
stripDataFromStatelike (Statelike s a) = a

stripI = stripIndexFromStatelike
stripD = stripDataFromStatelike

next :: (Ord s, Enum s, Bounded s, Eq a) => (Statelike s a) -> (Statelike s a)
next sl@(Statelike s a) = 
 if (||) (s == maxBound) (M.lookup (succ s) a == Nothing) 
  then sl 
  else Statelike (succ s) a

prev :: (Ord s, Enum s, Bounded s, Eq a) => (Statelike s a) -> (Statelike s a)
prev sl@(Statelike s a) = 
  if (||) (s == minBound) (M.lookup (pred s) a == Nothing)
  then sl 
  else Statelike (pred s) a

lift' :: M.Map s a -> Statelike s a
lift' m = Statelike (fst $ M.findMin m) m

-- PARTS OF GLOBAL, MISC. TYPE SYNONYMS

data Page = Latin1 | LatinSupp | LatinExtA | Box | Block | Braille deriving (Eq, Bounded, Enum, Show, Ord)
data Mode1 = Normal | ReplaceFGColor | ReplaceBGColor | ReplaceAllColor | ReplaceText deriving (Eq, Show)
data Mode2 = Stamp | Text | Line | Polygon | PolyFill deriving (Eq, Show)

type Mode = (Mode1, Mode2)
type Image = V.Vector (V.Vector Cell)  
type Cell = ([SGR], Char)  
type RelCursor = (Int, Int)

type Env = (Mode, Image) -- fear no danger

type Scheme = 
  Image -> 
  KeyCommand -> 
  StateT Global IO 
    (Either 
      [RelCursor] 
      (Image, Maybe Image)) -- FEAR NO DANGER 

-- UNITAL CLASS FOR PAINTING IMAGES

class Unital a where
  unit :: a
  (<~>) :: a -> a -> a

instance Unital Char where
  unit = '\NUL'
  a <~> b = if b == unit then a else b -- associative, AND idempotent!

instance Unital SGR where
  unit = Reset  
  x@(SetColor Foreground _ _) <~> y@(SetColor Foreground a b) = y
  x@(SetColor Background _ _) <~> y@(SetColor Background a b) = y
  x <~> y = if x == y then Reset else y

instance Unital [SGR] where
  unit = []
  a <~> b = filter (/= Reset) $ zipWith (<~>) a b -- this might cause problems.

instance (Unital a, Unital b) => Unital (a,b) where
  unit            = (,) unit unit
  (a,b) <~> (c,d) = (,) (a <~> c) (b <~> d)

-- COLORFUL CLASSES

class Colorlike a where
        toSGR :: a -> [SGR]

class Strippable a where
        strip :: a -> (ColorIntensity, Color)

data FGColor = FGColor !ColorIntensity !Color deriving (Eq, Show)
instance Colorlike FGColor where
        toSGR (FGColor a b) = [SetColor Foreground a b]
instance Strippable FGColor where
        strip (FGColor a b) = (a,b)

data BGColor = BGColor !ColorIntensity !Color deriving (Eq, Show)
instance Colorlike BGColor where
        toSGR (BGColor a b) = [SetColor Background a b]
instance Strippable BGColor where
        strip (BGColor a b) = (a,b)

data FullColor = FullColor FGColor BGColor
instance Colorlike FullColor where
        toSGR (FullColor a b) = toSGR a ++ toSGR b

-- DUMMY SGR BULLSHIT. MAYBE REDO THE SERIALIZING SOMETIME??????????????

instance Colorlike SGR where
        toSGR a = [a]

instance Colorlike [SGR] where
        toSGR = id

instance Serialize SGR where
  get = read <$> S.get
  put = S.put . show

-- CONFIGURATION THINGS

data KeyCommand = 
  Up | Down | KLeft | KRight | -- w a s d
  UpLeft | UpRight |         -- q e
  DownLeft | DownRight |     -- z c
  PrevFG | NextFG | -- i o
  PrevBG | NextBG | -- k l
  PrevTX | NextTX | -- y u
  PrevPG | NextPG | -- h j
  Save | Load |     -- S L
  Select | Dummy -- ???
  deriving (Eq,Show,Read)
