{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Graphics.FDL.GL

main = draw $ color purple star +> color pink (scale (1/3) circle)

