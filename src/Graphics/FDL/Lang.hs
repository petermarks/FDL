{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.FDL.Lang 
  ( FDL
  , Picture
  , Color
  , circle
  , star
  , color
  , rgb
  , red
  , green
  , blue
  , yellow
  , cyan
  , magenta
  , white
  , black
  , pink
  , purple
  , scale
  , move
  , rotate
  , time
  , (+>)
  , with
  , withSteps
  , grid
  , rotations
  , fromInteger
  , fromRational
  , (+)
  , (-)
  , (*)
  , (/)
  , ($)
  ) where

import Prelude (Bool, Maybe(..), Int, Integer, Rational, Double, Eq, (.), ($), map)
import qualified Prelude as P
import Data.Monoid
import Control.Applicative hiding (Const)
import Control.Monad.State

import Graphics.FDL.Lang.Impl

circle :: FDL Picture
circle = prim Circle

star :: FDL Picture
star = prim Star

color :: FDL Color -> FDL Picture -> FDL Picture
color = apply2 Color

rgb :: FDL Double -> FDL Double -> FDL Double -> FDL Color
rgb r g b = apply4 RGBA r g b 1

red     = rgb 1 0 0
green   = rgb 0 1 0
blue    = rgb 0 0 1
yellow  = rgb 1 1 0
cyan    = rgb 0 1 1
magenta = rgb 1 0 1
white   = rgb 1 1 1
black   = rgb 0 0 0
pink    = rgb 1 0.75 0.75
purple  = rgb 0.5 0 1

scale :: FDL Double -> FDL Picture -> FDL Picture
scale = apply2 Scale

move :: (FDL Double, FDL Double) -> FDL Picture -> FDL Picture
move (x, y) = apply2 Move (apply2 Pair x y) 

rotate :: FDL Double -> FDL Picture -> FDL Picture
rotate = apply2 Rotate

time :: FDL Double
time = prim Time

instance Monoid (FDL Picture) where
    mempty  = prim NOP
    mappend = apply2 Comp

(+>) :: FDL Picture -> FDL Picture -> FDL Picture
(+>) = mappend

infixr 5 +>

with :: (IxC a) => FDL a -> (FDL a -> FDL b) -> FDL b
with b f = Apply <$> lambda f <*> b 

withSteps :: FDL Double -> (FDL Double, FDL Double) -> (FDL Double -> FDL Picture) -> FDL Picture
withSteps steps (from, to) f = apply3 Steps steps (apply2 Pair from to) (lambda f)

grid :: (FDL Double, FDL Double) -> FDL Picture -> FDL Picture
grid (w, h) f = 
    scale (1/max w h) $ 
      withSteps h (1 - h, h - 1) $ \y -> 
        withSteps w (1 - w, w - 1) $ \x -> 
          move (x,y) f

rotations :: FDL Double -> FDL Picture -> FDL Picture
rotations n f = 
    withSteps n (0, 1 - 1 / n) $ \r -> 
      rotate r f

lambda :: (IxC a) => (FDL a -> FDL b) -> FDL (a -> b)
lambda f = do
    i <- get
    put (P.succ i)
    let v = Var ix i
    l <- f . return . VarRef $ v
    return $ Lambda v l

fromInteger  :: Integer  -> FDL Double
fromInteger  = prim . Const . P.fromInteger

fromRational :: Rational -> FDL Double
fromRational = prim . Const . P.fromRational

negate       :: FDL Double -> FDL Double
negate       = apply1 Negate

(+)          :: FDL Double -> FDL Double -> FDL Double
(+)          = apply2 Add

(/)          :: FDL Double -> FDL Double -> FDL Double
(/)          = apply2 Divide

(*)          :: FDL Double -> FDL Double -> FDL Double
(*)          = apply2 Mult

(-)          :: FDL Double -> FDL Double -> FDL Double
(-)          = apply2 Sub

max          :: FDL Double -> FDL Double -> FDL Double
max          = apply2 Max

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /

prim :: Prim a -> FDL a
prim = return . Prim 

apply1 :: Prim (a -> b) -> FDL a -> FDL b
apply1 f a = Apply (Prim f) <$> a

apply2 :: Prim (a -> b -> c) -> FDL a -> FDL b -> FDL c
apply2 f a b = Apply <$> apply1 f a <*> b

apply3 :: Prim (a -> b -> c -> d) -> FDL a -> FDL b -> FDL c -> FDL d
apply3 f a b c = Apply <$> apply2 f a b <*> c

apply4 :: Prim (a -> b -> c -> d -> e) -> FDL a -> FDL b -> FDL c -> FDL d -> FDL e
apply4 f a b c d = Apply <$> apply3 f a b c <*> d

instance Applicative (State s) where
    pure  = return
    (<*>) = ap
