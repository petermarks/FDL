module Graphics.FDL.Lang 
  ( FDL(..)
  , Picture
  , circle
  ) where

data Picture

data FDL a where
    Circle :: FDL Picture

circle :: FDL Picture
circle = Circle
