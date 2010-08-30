module Graphics.FDL.Lang 
  ( FDL(..)
  , Picture
  , Color
  , circle
  , star
  , color
  , rgb
  ) where

data Picture

data Color

data FDL a where
    Circle :: FDL Picture
    Star   :: FDL Picture
    Color  :: FDL Color -> FDL Picture -> FDL Picture
    RGBA   :: Double -> Double -> Double -> Double -> FDL Color

circle :: FDL Picture
circle = Circle

star :: FDL Picture
star = Star

color :: FDL Color -> FDL Picture -> FDL Picture
color = Color

rgb :: Double -> Double -> Double -> FDL Color
rgb r g b = RGBA r g b 1

