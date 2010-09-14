{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.FDL.Lang 
  ( FDL
  , Prim(..)
  , LCExpr(..)
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
  , withEach
  , Numeric(..)
  , ($)
  ) where

import Prelude (Integer, Rational, Double, (.), ($), map)
import qualified Prelude as P
import Data.Monoid

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

type FDL a = LCExpr a    

circle :: FDL Picture
circle = Prim Circle

star :: FDL Picture
star = Prim Star

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
time = Prim Time

instance Monoid (FDL Picture) where
    mempty  = Prim NOP
    mappend = apply2 Comp

(+>) :: FDL Picture -> FDL Picture -> FDL Picture
(+>) = mappend

infixr 5 +>

withEach :: [Double] -> (FDL Double -> FDL Picture) -> FDL Picture
withEach as f = mconcat . map (f . Prim . Const) $ as

class Numeric n where
    fromInteger  :: Integer  -> n
    fromRational :: Rational -> n
    negate       :: n -> n
    (/)          :: n -> n -> n

instance Numeric (LCExpr Double) where
    fromInteger  = Prim . Const . P.fromInteger
    fromRational = Prim . Const . P.fromRational
    negate       = apply1 Negate
    (/)          = apply2 Divide 

instance Numeric Double where
    fromInteger  = P.fromInteger
    fromRational = P.fromRational
    negate       = P.negate
    (/)          = (P./)

(<*>) :: LCExpr (a -> b) -> LCExpr a -> LCExpr b
(<*>) = Apply

infixl 5 <*>

apply1 :: Prim (a -> b) -> LCExpr a -> LCExpr b
apply1 f a = Prim f <*> a

apply2 :: Prim (a -> b -> c) -> LCExpr a -> LCExpr b -> LCExpr c
apply2 f a b = Prim f <*> a <*> b

apply3 :: Prim (a -> b -> c -> d) -> LCExpr a -> LCExpr b -> LCExpr c -> LCExpr d
apply3 f a b c = Prim f <*> a <*> b <*> c

apply4 :: Prim (a -> b -> c -> d -> e) -> LCExpr a -> LCExpr b -> LCExpr c -> LCExpr d -> LCExpr e
apply4 f a b c d = Prim f <*> a <*> b <*> c <*> d


