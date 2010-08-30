module Graphics.FDL.Backend.GL 
  ( compile
  , run
  ) where

import Prelude hiding (init)
import Graphics.UI.GLUT

import Graphics.FDL.Lang

compile :: FDL Picture -> IO (IO ())
compile Circle = return $ renderQuadric
    (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
    (Disk 0 1 36 1)
compile prim = error $ "can't compile primative"

run :: IO () -> IO ()
run prog = do
    getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, WithAlphaComponent, DoubleBuffered]
    win <- createWindow "FDL"
    displayCallback $= display prog
    idleCallback $= Just (postRedisplay (Just win))
    init
    mainLoop

init :: IO ()
init = do
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1) 1 (-1) 1

display :: IO () -> IO ()
display prog = do
    clear [ColorBuffer]
    prog
    swapBuffers
    
    
