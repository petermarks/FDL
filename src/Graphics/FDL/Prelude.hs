module Graphics.FDL.Prelude
  ( prelude
  ) where

import qualified Data.Map as M

import Graphics.FDL.Lang

prelude :: Environment
prelude = M.fromList
    [ ("circle",  expr $ Prim Circle)
    , ("star",    expr $ Prim Star)
    , ("color",   expr $ Prim Color)
    , ("red",     expr $ rgb 1 0 0)
    , ("green",   expr $ rgb 0 1 0)
    , ("blue",    expr $ rgb 0 0 1)
    , ("yellow",  expr $ rgb 1 1 0)
    , ("cyan",    expr $ rgb 0 1 1)
    , ("magenta", expr $ rgb 1 0 1)
    , ("white",   expr $ rgb 1 1 1)
    , ("black",   expr $ rgb 0 0 0)
    , ("pink",    expr $ rgb 1 0.75 0.75)
    , ("purple",  expr $ rgb 0.5 0 1)
    , ("size",    expr $ Prim Size)
    , ("rotate",  expr $ Prim Rotate)
    , ("time",    expr $ Prim Time)
    , ("+",       expr $ Prim Add)
    , ("-",       expr $ Prim Sub)
    , ("*",       expr $ Prim Mult)
    , ("/",       expr $ Prim Divide)
    , (";",       expr $ Prim Comp)
    ]

rgb :: Double -> Double -> Double -> LCExpr Color
rgb r g b = apply4 RGBA (Prim $ Const r) (Prim $ Const g) (Prim $ Const b) (Prim $ Const 1)

apply1 :: Prim (a -> b) -> LCExpr a -> LCExpr b
apply1 f a = Apply (Prim f) a

apply2 :: Prim (a -> b -> c) -> LCExpr a -> LCExpr b -> LCExpr c
apply2 f a b = Apply (apply1 f a) b

apply3 :: Prim (a -> b -> c -> d) -> LCExpr a -> LCExpr b -> LCExpr c -> LCExpr d
apply3 f a b c = Apply (apply2 f a b) c

apply4 :: Prim (a -> b -> c -> d -> e) -> LCExpr a -> LCExpr b -> LCExpr c -> LCExpr d -> LCExpr e
apply4 f a b c d = Apply (apply3 f a b c) d
