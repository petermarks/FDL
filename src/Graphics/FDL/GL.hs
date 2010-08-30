module Graphics.FDL.GL 
  ( module Graphics.FDL.Lang
  , draw
  ) where

import Graphics.FDL.Lang
import Graphics.FDL.Backend.GL

draw :: FDL Picture -> IO ()
draw picture = do
    prog <- compile picture
    run prog



