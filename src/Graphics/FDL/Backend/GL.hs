module Graphics.FDL.Backend.GL 
  ( compile
  , run
  ) where

import Data.Time
import Data.IORef
import Control.Monad.Reader
import Control.Applicative
import Graphics.UI.GLUT

import qualified Graphics.FDL.Lang as L

type family Output a :: *

type instance Output L.Picture = ()
type instance Output L.Color   = Color4 Double
type instance Output Double    = Double

data CompileContext = CompileContext
    { ccStart :: UTCTime
    , ccTimeRef  :: IORef Double
    }

type Compiler a = Reader CompileContext (IO a)

compile :: L.FDL L.Picture -> IO (IO ())
compile pic = do
    start   <- getCurrentTime
    timeRef <- newIORef 0
    return $ runReader (compileProg pic) (CompileContext start timeRef)

compileProg :: L.FDL L.Picture -> Compiler ()
compileProg pic = do
    initAction <- initFrame
    picAction  <- comp pic
    return $ initAction >> picAction

initFrame :: Compiler ()
initFrame = do
    start   <- asks ccStart
    timeRef <- asks ccTimeRef
    return $ do
      now <- getCurrentTime
      writeIORef timeRef . realToFrac $ diffUTCTime now start

comp :: L.FDL a -> Compiler (Output a)
comp L.NOP = return $ return ()
comp L.Circle = return $ renderQuadric
    (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
    (Disk 0 1 36 1)
comp L.Star = return $ renderPrimitive TriangleStrip . mapM_ vertex $
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
comp (L.Color (L.RGBA r g b a) pic) = do
    ra <- comp r
    ga <- comp g
    ba <- comp b
    aa <- comp a
    pica <- comp pic    
    return $ preservingAttrib [ColorBufferAttributes] $ do
      c <- Color4 <$> ra <*> ga <*> ba <*> aa
      color c
      pica
comp (L.Scale factor pic) = do
    factora <- fmap realToFrac <$> comp factor :: Compiler GLdouble
    pica <- comp pic
    return $ preservingMatrix $ do
      s <- factora
      scale s s 1
      pica
comp (L.Move (x,y) pic) = do
    xa <- fmap realToFrac <$> comp x :: Compiler GLdouble
    ya <- fmap realToFrac <$> comp y :: Compiler GLdouble
    pica <- comp pic
    return $ preservingMatrix $ do
      v <- Vector3 <$> xa <*> ya <*> pure 0
      translate v
      pica
comp (L.Rotate a pic) = do
    aa <- fmap realToFrac <$> comp a :: Compiler GLdouble
    pica <- comp pic
    return $ preservingMatrix $ do
      degrees <- (* 360) <$> aa
      rotate degrees (Vector3 0 0 1)
      pica
comp (L.Const v) =
    return . return $ v
comp (L.Negate a) = do
    aa <- comp a
    return $
      negate <$> aa
comp (L.Divide a b) = do
    aa <- comp a
    ba <- comp b
    return $
      (/) <$> aa <*> ba
comp (L.Time) = do
    timeRef <- asks ccTimeRef
    return $
      readIORef timeRef
comp (L.Comp pics) = 
    sequence_ <$> mapM comp pics

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
    
    
