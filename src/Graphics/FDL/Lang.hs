module Graphics.FDL.Lang 
  ( FDL(..)
  , Picture
  , circle
  , star
  ) where

data Picture

data FDL a where
    Circle :: FDL Picture
    Star   :: FDL Picture

circle :: FDL Picture
circle = Circle

star :: FDL Picture
star = Star
