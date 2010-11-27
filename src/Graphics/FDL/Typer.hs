module Graphics.FDL.Typer
    ( TyperError(..)
    , TyperResult(..)
    , typeProg
    ) where

import Control.Applicative
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

instance Applicative TyperResult where
    pure = TyperSuccess
    (TyperSuccess f)  <*> (TyperSuccess v)  = TyperSuccess (f v)
    (TyperErrors es1) <*> (TyperErrors es2) = TyperErrors (es1 ++ es2)
    (TyperSuccess _)  <*> (TyperErrors es2) = TyperErrors (es2)
    (TyperErrors es1) <*> (TyperSuccess _)  = TyperErrors (es1)

instance Monad TyperResult where
    return = TyperSuccess
    (TyperErrors es) >>= _ = TyperErrors es
    (TyperSuccess v) >>= f = f v

type Typer a b = a -> State Environment (TyperResult b)


----------------------------------------------------------------------
-- Typers
----------------------------------------------------------------------

tProgram :: Typer Program (LCExpr Picture)
tProgram (Program e@(PE pos _)) = do
    rExpr <- tExpression e
    return $ unwrap =<< rExpr
    where
      unwrap :: Expr -> TyperResult (LCExpr Picture)
      unwrap (Expr twitExpr expr) = case twitExpr .=. Picture_ of
        Nothing  -> TyperErrors  [TyperError ("Incorrect type: expected Picture, but found " ++ showTWit twitExpr) pos]
        Just TEq -> TyperSuccess expr

tExpression :: Typer (ProgElem Expression) Expr
tExpression (PE pos (Reference ref)) = do
    maybe err TyperSuccess <$> gets (M.lookup ref)
    where
      err = TyperErrors [TyperError ("Unknown name: " ++ ref) pos]
tExpression (PE pos _) = return $ TyperErrors [TyperError "Don't know how to type expression" pos]

--      Just Variable -> do
--        let var = VarRef $ Var twit ref
--        modify (insert ref (Expr twit var))
--        return $ TyperSuccess var


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

typeProg :: Program -> TyperResult (LCExpr Picture)
typeProg = flip evalState prelude . tProgram 
