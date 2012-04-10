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

type instance Output L.Picture = IO ()
type instance Output L.Color   = IO (Color4 Double)
type instance Output Double    = IO Double
type instance Output (a, b)    = (Output a, Output b)
type instance Output (a -> b)  = Output a -> Output b

data CompileContext = CompileContext
    { ccStart   :: UTCTime
    , ccTimeRef :: IORef Double
    }

type Compiler a = Reader CompileContext a

compile :: L.LCExpr L.Picture -> IO (IO ())
compile pic = do
    start   <- getCurrentTime
    timeRef <- newIORef 0
    return $ runReader (compileProg pic) (CompileContext start timeRef)

compileProg :: L.LCExpr L.Picture -> Compiler (IO ())
compileProg pic = do
    initAction <- initFrame
    picAction  <- compExpr . L.lcToCL $ pic
    return $ initAction >> picAction

initFrame :: Compiler (IO ())
initFrame = do
    start   <- asks ccStart
    timeRef <- asks ccTimeRef
    return $ do
      now <- getCurrentTime
      writeIORef timeRef . realToFrac $ diffUTCTime now start

compExpr :: L.CLExpr a -> Compiler (Output a)
compExpr (L.P v)     = comp v
compExpr (L.S)       = return $ \x y z -> x z (y z)
compExpr (L.K)       = return const
compExpr (L.I)       = return id
compExpr (L.A fe ae) = do
    f  <- compExpr fe
    aa <- compExpr ae
    return $ f aa
compExpr (L.V _)     = error "Unbound variable found in expression."

comp :: L.Prim a -> Compiler (Output a)
comp L.NOP = return $ return ()
comp L.Circle = 
    return $ renderQuadric
      (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
      (Disk 0 1 36 1)
comp L.Star = 
    return $ renderPrimitive TriangleStrip . mapM_ vertex $
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
comp L.Square =
    return $ rect (Vertex2 (-1::GLdouble) (-1)) (Vertex2 1 1)
comp L.Color =
    return $ \ca pica -> preservingAttrib [ColorBufferAttributes] $ do
      c <- ca
      color c
      pica
comp L.RGBA =
    return $ \ra ga ba aa ->
      Color4 <$> ra <*> ga <*> ba <*> aa
comp L.Size =
    return $ \sa pica -> preservingMatrix $ do
      s <- realToFrac <$> sa :: IO GLdouble
      scale s s 1
      pica
comp L.Scale =
    return $ \(wa, ha) pica -> preservingMatrix $ do
      w <- realToFrac <$> wa :: IO GLdouble
      h <- realToFrac <$> ha :: IO GLdouble
      scale w h 1
      pica
comp L.Move =
    return $ \(xa, ya) pica -> preservingMatrix $ do
      x <- realToFrac <$> xa :: IO GLdouble
      y <- realToFrac <$> ya :: IO GLdouble
      translate $ Vector3 x y 0
      pica
comp L.Rotate =
    return $ \aa pica -> preservingMatrix $ do
      a <- (* 360) . realToFrac <$> aa :: IO GLdouble
      rotate a (Vector3 0 0 1)
      pica
comp (L.Const v) =
    return . return $ v
comp L.Negate =
    return $ \aa ->
      negate <$> aa
comp L.Add =
    return $ \aa ba ->
      (+) <$> aa <*> ba
comp L.Sub =
    return $ \aa ba ->
      (-) <$> aa <*> ba
comp L.Mult =
    return $ \aa ba ->
      (*) <$> aa <*> ba
comp L.Divide =
    return $ \aa ba ->
      (/) <$> aa <*> ba
comp L.Max =
    return $ \aa ba ->
      max <$> aa <*> ba
comp L.Time = do
    timeRef <- asks ccTimeRef
    return $
      readIORef timeRef
comp L.Pulse = do
    timeRef <- asks ccTimeRef
    return $
      (0.5 +) . (0.5 *) . sin . (pi * 2 *) <$> readIORef timeRef
comp L.Speed = do
    timeRef <- asks ccTimeRef
    return $ \sa aa -> do
      s <- sa
      withModifiedTime timeRef (* s) aa
comp L.Delay = do
    timeRef <- asks ccTimeRef
    return $ \da aa -> do
      d <- da
      withModifiedTime timeRef (subtract d) aa
comp L.Pair =
    return $ \aa ba ->
      (aa, ba)
comp L.Steps =
    return $ \stepsa (froma, toa) f -> do
      steps <- stepsa
      from  <- froma
      to    <- toa
      let next = from + (to - from) / (steps - 1)
      mapM_ (f . return) $ enumFromThenTo from next to
comp L.Comp =
    return $ \picaa picba ->
      picaa >> picba

withModifiedTime :: IORef Double -> (Double -> Double) -> IO a -> IO a
withModifiedTime timeRef f aa = do
    t <- readIORef timeRef
    writeIORef timeRef (f t)
    a <- aa
    writeIORef timeRef t
    return a

run :: IO () -> IO ()
run prog = do
    initialWindowSize $= Size 600 600
    getArgsAndInitialize
    actionOnWindowClose $= ContinueExectuion
    initialDisplayMode  $= [RGBAMode, WithAlphaComponent, DoubleBuffered]
    win <- createWindow "FDL"
    blend               $= Enabled
    blendEquation       $= FuncAdd
    blendFunc           $= (SrcAlpha, OneMinusSrcAlpha)
    displayCallback     $= display prog
    idleCallback        $= Just (postRedisplay (Just win))
    reshapeCallback     $= Just reshape
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
    
    
