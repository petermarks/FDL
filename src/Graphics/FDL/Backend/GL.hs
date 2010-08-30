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
compile Star = return . renderPrimitive TriangleStrip . mapM_ vertex $
    [ point 0
    , point 2
    , centre
    , point 4
    , point 1
    , centre
    , point 3
    , point 0
    ]
    where
      point :: Double -> Vertex2 Double
      point theta = Vertex2 (0 - sin (theta * pi * 2 / 5)) (cos (theta * pi * 2 / 5))
      centre :: Vertex2 Double
      centre = Vertex2 0 0
     
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
    
    
