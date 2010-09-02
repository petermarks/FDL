{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Graphics.FDL.GL

main = draw $
    rotate time $
      color purple star

