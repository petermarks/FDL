module Graphics.FDL.Lang
  ( Prim(..)
  , Var(..)
  , LCExpr(..)
  , CLExpr(..)
  , Picture
  , Color
  , TWit(..)
  , (:=:)(..)
  , Witnessed(..)
  , Expr(..)
  , Environment
  , (.=.)
  , expr
  , lcToCL
  , showTWit
  ) where

import qualified Data.Map as M

data Picture

data Color

data TWit :: * -> * where
    Picture_ :: TWit Picture
    Color_   :: TWit Color
    Double_  :: TWit Double
    Pair_    :: TWit a -> TWit b -> TWit (a, b)
    Func_    :: TWit a -> TWit b -> TWit (a -> b)

class    Witnessed t       where twit :: TWit t
instance Witnessed Picture where twit = Picture_
instance Witnessed Color   where twit = Color_
instance Witnessed Double  where twit = Double_
instance (Witnessed a, Witnessed b) => Witnessed (a,   b) where twit = Pair_ twit twit
instance (Witnessed a, Witnessed b) => Witnessed (a -> b) where twit = Func_ twit twit

data (:=:) :: * -> * -> * where
    TEq :: a :=: a

(.=.) :: TWit a -> TWit b -> Maybe (a :=: b)
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
    Square :: Prim Picture
    Color  :: Prim (Color -> Picture -> Picture)
    RGBA   :: Prim (Double -> Double -> Double -> Double -> Color)
    Size   :: Prim (Double -> Picture -> Picture)
    Scale  :: Prim ((Double, Double) -> Picture -> Picture)
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
    Pulse  :: Prim Double
    Speed  :: Prim (Double -> Picture -> Picture)
    Delay  :: Prim (Double -> Picture -> Picture)
    Pair   :: Prim (Double -> Double -> (Double, Double))
    Steps  :: Prim (Double -> (Double, Double) -> (Double -> Picture) -> Picture)
    Comp   :: Prim (Picture -> Picture -> Picture)

data Var a = Var (TWit a) String

data LCExpr :: * -> * where
    Prim   :: Prim a -> LCExpr a
    Apply  :: LCExpr (a -> b) -> LCExpr a -> LCExpr b
    Lambda :: Var a -> LCExpr b -> LCExpr (a -> b)
    VarRef :: Var a -> LCExpr a

data CLExpr :: * -> * where
    P :: Prim a -> CLExpr a
    V :: Var  a -> CLExpr a
    S :: CLExpr ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: CLExpr (a -> b -> a)
    I :: CLExpr (a -> a)
    A :: CLExpr (a -> b) -> CLExpr a -> CLExpr b

data Expr
  = forall a . Expr (TWit a) (LCExpr a)
  -- | Variable -- not yet typed

expr :: (Witnessed a) => (LCExpr a) -> Expr
expr e = Expr twit e

type Environment = M.Map String Expr
-- TODO may need to become stack of maps: [M.Map String Expr]

lcToCL :: LCExpr a -> CLExpr a
lcToCL (Prim   a)   = P a
lcToCL (VarRef v)   = V v
lcToCL (Apply  f a) = A (lcToCL f) (lcToCL a)
lcToCL (Lambda v l) = lambdaToCL v (lcToCL l)

lambdaToCL :: Var a -> CLExpr b -> CLExpr (a -> b)
lambdaToCL v l | not $ l `hasVar` v = A K l
lambdaToCL (Var vWit _) (V (Var vrWit _))  = case vWit .=. vrWit of
    Nothing  -> error "Variable should have been eliminated in lambdaToCL"
    Just TEq -> I     -- var must match otherwise it will have been eliminated
lambdaToCL i (A f v) = A (A S (lambdaToCL i f)) (lambdaToCL i v)
lambdaToCL _ _       = error "All cases should have been handled by previous clauses in lambdaToCL"

hasVar :: CLExpr a -> Var b -> Bool
hasVar (V (Var _ vri)) (Var _ vi) = vi == vri
hasVar (A f a)         v          = f `hasVar` v || a `hasVar` v
hasVar _               _          = False

showTWit :: TWit s -> String
showTWit Picture_ = "Picture"
showTWit Color_   = "Color"
showTWit Double_  = "Number"
showTWit (Pair_ l r) = '(' : showTWit l ++ ',' : showTWit r ++ ")"
showTWit (Func_ l@(Func_ {}) r) = '(' : showTWit l ++ ") -> " ++ showTWit r
showTWit (Func_ l            r) =       showTWit l ++  " -> " ++ showTWit r

