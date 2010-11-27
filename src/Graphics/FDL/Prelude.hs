module Graphics.FDL.Prelude where

import qualified Data.Map as M

import Graphics.FDL.Lang

prelude :: Environment
prelude = M.fromList
    [ ("circle", expr $ Prim Circle)
    , ("color",  expr $ Prim Color)
    ]
