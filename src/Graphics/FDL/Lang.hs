{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.FDL.Lang 
  ( FDL
  , Prim(..)
  , LCExpr(..)
  , CLExpr(..)
  , Picture
  , Color
  , toCL
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
  , withEach
  , Numeric(..)
  , ($)
  ) where

import Prelude (Bool, Maybe(..), Int, Integer, Rational, Double, Eq, (.), ($), map, error)
import qualified Prelude as P
import Data.Monoid
import Control.Applicative hiding (Const)
import Control.Monad.State

data Picture

data Color

data Ix :: * -> * where
    Picture_ :: Ix Picture
    Color_   :: Ix Color
    Double_  :: Ix Double
    Pair_    :: Ix a -> Ix b -> Ix (a, b)
    Func_    :: Ix a -> Ix b -> Ix (a -> b)

class    IxC t       where ix :: Ix t
instance IxC Picture where ix = Picture_
instance IxC Color   where ix = Color_
instance IxC Double  where ix = Double_
instance (IxC a, IxC b) => IxC (a,   b) where ix = Pair_ ix ix
instance (IxC a, IxC b) => IxC (a -> b) where ix = Func_ ix ix

data (:=:) :: * -> * -> * where
    TEq :: a :=: a

(.=.) :: Ix a -> Ix b -> Maybe (a :=: b)
Picture_    .=. Picture_    = Just TEq
Color_      .=. Color_      = Just TEq
Double_     .=. Double_     = Just TEq
(Pair_ a b) .=. (Pair_ c d) = 
    case (a .=. c, b .=. d) of
      (Just TEq, Just TEq) -> Just TEq
      _                    -> Nothing
(Func_ a b) .=. (Func_ c d) = 
    case (a .=. c, b .=. d) of
      (Just TEq, Just TEq) -> Just TEq
      _                    -> Nothing
_          .=. _            = Nothing     

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

data Var a = Var (Ix a) Int

data LCExpr :: * -> * where
    Prim   :: Prim a -> LCExpr a
    Apply  :: LCExpr (a -> b) -> LCExpr a -> LCExpr b
    Lambda :: Var a -> LCExpr b -> LCExpr (a -> b)
    VarRef :: Var a -> LCExpr a

type FDL a = State Int (LCExpr a)

data CLExpr :: * -> * where
    P :: Prim a -> CLExpr a
    V :: Var  a -> CLExpr a
    S :: CLExpr ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: CLExpr (a -> b -> a)
    I :: CLExpr (a -> a)
    A :: CLExpr (a -> b) -> CLExpr a -> CLExpr b

toCL :: FDL a -> CLExpr a
toCL = lcToCL . P.flip evalState P.minBound

lcToCL :: LCExpr a -> CLExpr a
lcToCL (Prim   a)   = P a
lcToCL (VarRef v)   = V v
lcToCL (Apply  f a) = A (lcToCL f) (lcToCL a)
lcToCL (Lambda v l) = lambdaToCL v (lcToCL l)

lambdaToCL :: Var a -> CLExpr b -> CLExpr (a -> b)
lambdaToCL v l | P.not $ l `hasVar` v = A K l
lambdaToCL (Var vix _) (V (Var vrix _))  = case vix .=. vrix of
    Nothing  -> error "Variable should have been eliminated"
    Just TEq -> I     -- var must match otherwise it will have been eliminated
lambdaToCL i (A f v) = A (A S (lambdaToCL i f)) (lambdaToCL i v)

hasVar :: CLExpr a -> Var b -> Bool
hasVar (V (Var _ vri)) (Var _ vi) = vi P.== vri
hasVar (A f a)         v          = f `hasVar` v P.|| a `hasVar` v
hasVar _               _          = P.False

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
with b f = do
    i <- get
    put (P.succ i)
    let v = Var ix i
    l <- f . return . VarRef $ v
    Apply (Lambda v l) <$> b 

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
