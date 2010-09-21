module Graphics.FDL.Lang.Impl
  ( FDL
  , Prim(..)
  , Var(..)
  , LCExpr(..)
  , CLExpr(..)
  , Picture
  , Color
  , Ix(..)
  , (:=:)(..)
  , IxC(..)
  , toCL
  ) where

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
    Add    :: Prim (Double -> Double -> Double)
    Sub    :: Prim (Double -> Double -> Double)
    Mult   :: Prim (Double -> Double -> Double)
    Divide :: Prim (Double -> Double -> Double)
    Max    :: Prim (Double -> Double -> Double)
    Time   :: Prim Double
    Pair   :: Prim (a -> b -> (a, b))
    Steps  :: Prim (Double -> (Double, Double) -> (Double -> Picture) -> Picture)
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
toCL = lcToCL . flip evalState 0

lcToCL :: LCExpr a -> CLExpr a
lcToCL (Prim   a)   = P a
lcToCL (VarRef v)   = V v
lcToCL (Apply  f a) = A (lcToCL f) (lcToCL a)
lcToCL (Lambda v l) = lambdaToCL v (lcToCL l)

lambdaToCL :: Var a -> CLExpr b -> CLExpr (a -> b)
lambdaToCL v l | not $ l `hasVar` v = A K l
lambdaToCL (Var vix _) (V (Var vrix _))  = case vix .=. vrix of
    Nothing  -> error "Variable should have been eliminated"
    Just TEq -> I     -- var must match otherwise it will have been eliminated
lambdaToCL i (A f v) = A (A S (lambdaToCL i f)) (lambdaToCL i v)

hasVar :: CLExpr a -> Var b -> Bool
hasVar (V (Var _ vri)) (Var _ vi) = vi == vri
hasVar (A f a)         v          = f `hasVar` v || a `hasVar` v
hasVar _               _          = False

