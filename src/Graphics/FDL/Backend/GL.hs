module Graphics.FDL.Backend.GL 
  ( compile
  , run
  ) where

import Graphics.UI.GLUT

import qualified Graphics.FDL.Lang as L

compile :: L.FDL L.Picture -> IO (IO ())
compile = return . compilePic

compilePic :: L.FDL L.Picture -> IO()
compilePic L.NOP = return ()
compilePic L.Circle = renderQuadric
    (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
    (Disk 0 1 36 1)
compilePic L.Star = renderPrimitive TriangleStrip . mapM_ vertex $
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
compilePic (L.Color (L.RGBA r g b a) pic) = preservingAttrib [ColorBufferAttributes] $ do
    color $ Color4 r g b a
    compilePic pic
compilePic (L.Scale factor pic) = preservingMatrix $ do
    scale s s 1
    compilePic pic
    where
      s :: GLdouble
      s = realToFrac factor
compilePic (L.Move (x,y) pic) = preservingMatrix $ do
    translate $ Vector3 xx yy 0
    compilePic pic
    where
      xx, yy :: GLdouble
      xx = realToFrac x
      yy = realToFrac y
compilePic (L.Comp pics) = mapM_ compilePic pics

run :: IO () -> IO ()
run prog = do
    initialWindowSize $= Size 600 600
    getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, WithAlphaComponent, DoubleBuffered]
    win <- createWindow "FDL"
    displayCallback $= display prog
    idleCallback    $= Just (postRedisplay (Just win))
    reshapeCallback $= Just reshape
    mainLoop

reshape :: Size -> IO ()
reshape size@(Size w h) = do
    matrixMode $= Projection
    loadIdentity
    if w <= h
      then ortho2D (-1) 1 (-1 * hh/ww) (hh/ww)
      else ortho2D (-1 * ww/hh) (ww/hh) (-1) 1
    matrixMode $= Modelview 0
    viewport $= ((Position 0 0), size)
    where
      ww = fromIntegral w
      hh = fromIntegral h

display :: IO () -> IO ()
display prog = do
    clear [ColorBuffer]
    prog
    swapBuffers
    
    
