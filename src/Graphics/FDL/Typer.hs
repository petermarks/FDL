module Graphics.FDL.Typer
    ( TyperError(..)
    , TyperResult(..)
    , typeProg
    ) where

import Control.Applicative hiding (Const)
import Control.Monad.State
import qualified Data.Map as M

import Graphics.FDL.Lang
import Graphics.FDL.Parser
import Graphics.FDL.Prelude


----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

data TyperError = TyperError
    { teMessage :: String
    , tePos     :: Pos
    }
    deriving Show

data TyperResult a = TyperErrors [TyperError] | TyperSuccess a

instance Functor TyperResult where
    fmap _ (TyperErrors es) = TyperErrors es
    fmap f (TyperSuccess v) = TyperSuccess (f v)

instance Monad TyperResult where
    return = TyperSuccess
    (TyperErrors es) >>= _ = TyperErrors es
    (TyperSuccess v) >>= f = f v

type Typer a b = a -> StateT Environment TyperResult b

instance Applicative (StateT Environment TyperResult) where
    pure = return
    mf <*> ma = StateT $ \s -> case runStateT mf s of
      TyperErrors es1 -> case runStateT ma s of
        TyperErrors es2 -> TyperErrors (es1 ++ es2)
        _               -> TyperErrors es1
      TyperSuccess (f, s') -> case runStateT ma s' of
        TyperErrors  es2      -> TyperErrors es2
        TyperSuccess (a, s'') -> TyperSuccess (f a, s'')

err :: String -> Typer Pos a
err msg pos = StateT $ \_ -> TyperErrors [TyperError msg pos]


----------------------------------------------------------------------
-- Typers
----------------------------------------------------------------------

tProgram :: Typer Program (LCExpr Picture)
tProgram (Program e) =
    tExpression e >>= unwrap
    where
      unwrap :: Typer Expr (LCExpr Picture)
      unwrap (Expr twitExpr expr) = case twitExpr .=. Picture_ of
        Nothing  -> err ("Incorrect type: expected Picture, but found " ++ showTWit twitExpr) (pePos e)
        Just TEq -> return expr

tExpression :: Typer (ProgElem Expression) Expr
tExpression (PE pos (Reference ref)) =
    gets (M.lookup ref) >>= maybe (err ("Unknown name: " ++ ref) pos) return 
tExpression (PE _ (Number num)) = return . expr . Prim . Const $ num
tExpression (PE _ (Application ef ea)) = do
    (,) <$> tExpression ef <*> tExpression ea >>= apply
    where
      apply :: Typer (Expr, Expr) Expr
      apply (Expr (Func_ twitArg twitResult) f, Expr twitA a) = case twitArg .=. twitA of
        Just TEq -> return . Expr twitResult $ Apply f a
        Nothing  -> err ("Incorrect type: expected " ++ showTWit twitArg ++ ", but found " ++ showTWit twitA) (pePos ea)
      apply (Expr twitF _, _) =
        err ("Function expected, but found " ++ showTWit twitF) (pePos ef)
tExpression (PE pos _) = err "Don't know how to type expression" pos

--      Just Variable -> do
--        let var = VarRef $ Var twit ref
--        modify (insert ref (Expr twit var))
--        return $ TyperSuccess var


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

typeProg :: Program -> TyperResult (LCExpr Picture)
typeProg = flip evalStateT prelude . tProgram 
