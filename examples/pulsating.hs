{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Graphics.FDL.GL

main = draw $ 
    speed (1/3) $
      redSquare +> delay (1/2) blueSquare

redSquare =
    rotate time $ scale pulse $ color (rgba 1 0 0 0.5) square

blueSquare =
    rotate (-time) $ scale pulse $ color (rgba 0 0 1 0.5) square

