{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Graphics.FDL.GL

main = draw $
    with (scale (1/2) $ rotate time $ color red star) $ \s -> 
      move (-1/2, 0) s +> move (1/2, 0) s

