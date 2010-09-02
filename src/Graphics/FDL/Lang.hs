{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.FDL.Lang 
  ( FDL(..)
  , Picture
  , Color
  , circle
  , star
  , color
  , rgb
  , red
  , green
  , blue
  , yellow
  , cyan
  , magenta
  , white
  , black
  , pink
  , purple
  , scale
  , move
  , rotate
  , time
  , (+>)
  , withEach
  , Numeric(..)
  , ($)
  ) where

import Prelude (Integer, Rational, Double, (.), ($), map)
import qualified Prelude as P
import Data.Monoid

data Picture

data Color

data FDL a where
    NOP    :: FDL Picture
    Circle :: FDL Picture
    Star   :: FDL Picture
    Color  :: FDL Color -> FDL Picture -> FDL Picture
    RGBA   :: FDL Double -> FDL Double -> FDL Double -> FDL Double -> FDL Color
    Scale  :: FDL Double -> FDL Picture -> FDL Picture
    Move   :: (FDL Double, FDL Double) -> FDL Picture -> FDL Picture
    Rotate :: FDL Double -> FDL Picture -> FDL Picture
    Const  :: Double -> FDL Double
    Negate :: FDL Double -> FDL Double
    Divide :: FDL Double -> FDL Double -> FDL Double
    Time   :: FDL Double
    Comp   :: [FDL Picture] -> FDL Picture

circle :: FDL Picture
circle = Circle

star :: FDL Picture
star = Star

color :: FDL Color -> FDL Picture -> FDL Picture
color = Color

rgb :: FDL Double -> FDL Double -> FDL Double -> FDL Color
rgb r g b = RGBA r g b 1

red     = rgb 1 0 0
green   = rgb 0 1 0
blue    = rgb 0 0 1
yellow  = rgb 1 1 0
cyan    = rgb 0 1 1
magenta = rgb 1 0 1
white   = rgb 1 1 1
black   = rgb 0 0 0
pink    = rgb 1 0.75 0.75
purple  = rgb 0.5 0 1

scale :: FDL Double -> FDL Picture -> FDL Picture
scale = Scale

move :: (FDL Double, FDL Double) -> FDL Picture -> FDL Picture
move = Move

rotate :: FDL Double -> FDL Picture -> FDL Picture
rotate = Rotate

time :: FDL Double
time = Time

instance Monoid (FDL Picture) where
    mempty      = NOP
    mappend a b = Comp [a, b]
    mconcat as  = Comp as

(+>) :: FDL Picture -> FDL Picture -> FDL Picture
(+>) = mappend

infixr 5 +>

withEach :: [Double] -> (FDL Double -> FDL Picture) -> FDL Picture
withEach as f = Comp . map (f . Const) $ as

class Numeric n where
    fromInteger  :: Integer  -> n
    fromRational :: Rational -> n
    negate       :: n -> n
    (/)          :: n -> n -> n

instance Numeric (FDL Double) where
    fromInteger  = Const . P.fromInteger
    fromRational = Const . P.fromRational
    negate       = Negate
    (/)          = Divide 

instance Numeric Double where
    fromInteger  = P.fromInteger
    fromRational = P.fromRational
    negate       = P.negate
    (/)          = (P./)



