module Main where

import Graphics.FDL.GL

main = draw $ scale (1/10) $ 
    withEach [-9,-7..9] $ \y -> 
      withEach [-9,-7..9] $ \x -> 
        move (x,y) $
          color purple star

