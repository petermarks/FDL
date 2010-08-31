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
  , (+>)
  ) where

import Data.Monoid

data Picture

data Color

data FDL a where
    NOP    :: FDL Picture
    Circle :: FDL Picture
    Star   :: FDL Picture
    Color  :: FDL Color -> FDL Picture -> FDL Picture
    RGBA   :: Double -> Double -> Double -> Double -> FDL Color
    Scale  :: Double -> FDL Picture -> FDL Picture
    Comp   :: [FDL Picture] -> FDL Picture

circle :: FDL Picture
circle = Circle

star :: FDL Picture
star = Star

color :: FDL Color -> FDL Picture -> FDL Picture
color = Color

rgb :: Double -> Double -> Double -> FDL Color
rgb r g b = RGBA r g b 1

red     = rgb 1 0 0
green   = rgb 0 1 0
blue    = rgb 0 0 1
yellow  = rgb 1 1 0
cyan    = rgb 0 1 1
magenta = rgb 1 0 1
white   = rgb 1 1 1
black   = rgb 0 0 0
pink    = rgb 1 (3/4) (3/4)
purple  = rgb (1/2) 0 1

scale :: Double -> FDL Picture -> FDL Picture
scale = Scale

instance Monoid (FDL Picture) where
    mempty      = NOP
    mappend a b = Comp [a, b]
    mconcat as  = Comp as

(+>) :: FDL Picture -> FDL Picture -> FDL Picture
(+>) = mappend

infixr 5 +>

