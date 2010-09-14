{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.FDL.Lang 
  ( FDL
  , Prim(..)
  , LCExpr(..)
  , Picture
  , Color
  , runLang
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
  , withEach
  , Numeric(..)
  , ($)
  ) where

import Prelude (Integer, Rational, Double, (.), ($), map)
import qualified Prelude as P
import Data.Monoid
import Control.Applicative hiding (Const)
import Control.Monad.State

data Picture

data Color

data Prim :: * -> * where
    NOP    :: Prim Picture
    Circle :: Prim Picture
    Star   :: Prim Picture
    Color  :: Prim (Color -> Picture -> Picture)
    RGBA   :: Prim (Double -> Double -> Double -> Double -> Color)
    Scale  :: Prim (Double -> Picture -> Picture)
    Move   :: Prim ((Double, Double) -> Picture -> Picture)
    Rotate :: Prim (Double -> Picture -> Picture)
    Const  :: Double -> Prim Double
    Negate :: Prim (Double -> Double)
    Divide :: Prim (Double -> Double -> Double)
    Time   :: Prim Double
    Pair   :: Prim (a -> b -> (a, b))
    Comp   :: Prim (Picture -> Picture -> Picture)

data LCExpr :: * -> * where
    Prim  :: Prim a -> LCExpr a
    Apply :: LCExpr (a -> b) -> LCExpr a -> LCExpr b

type FDL a = State P.Int (LCExpr a)

runLang :: FDL a -> LCExpr a
runLang = P.flip evalState P.minBound

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

withEach :: [Double] -> (FDL Double -> FDL Picture) -> FDL Picture
withEach as f = mconcat . map (f . prim . Const) $ as

class Numeric n where
    fromInteger  :: Integer  -> n
    fromRational :: Rational -> n
    negate       :: n -> n
    (/)          :: n -> n -> n

instance Numeric (FDL Double) where
    fromInteger  = prim . Const . P.fromInteger
    fromRational = prim . Const . P.fromRational
    negate       = apply1 Negate
    (/)          = apply2 Divide 

instance Numeric Double where
    fromInteger  = P.fromInteger
    fromRational = P.fromRational
    negate       = P.negate
    (/)          = (P./)

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
