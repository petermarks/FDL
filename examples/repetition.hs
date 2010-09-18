{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Graphics.FDL.GL

main = draw $ 
    grid (10, 10) $
      color purple star

