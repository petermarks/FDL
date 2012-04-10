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
tProgram (Program e ds) = do
    tDefinitions  ds
    tExpressionAs Picture_ e

tExpression :: Typer (ProgElem Expression) Expr
tExpression (PE pos (Reference ref)) =
    gets (M.lookup ref) >>= maybe (err ("Unknown name: " ++ ref) pos) return 
tExpression (PE _ (Number num)) = return . expr . Prim . Const $ num
tExpression (PE _ (Pairing ea eb)) = do
    pair <$> tExpressionAs Double_ ea <*> tExpressionAs Double_ eb
    where
      pair a b = Expr (Pair_ Double_ Double_) (Apply (Apply (Prim Pair) a) b)
tExpression (PE _ (Application ef ea)) = do
    tExpression ef >>= apply
    where
      apply :: Typer Expr Expr
      apply (Expr (Func_ twitArg twitResult) f) = 
        Expr twitResult . Apply f <$> tExpressionAs twitArg ea
      apply (Expr twitF _) =
        err ("Function expected, but found " ++ showTWit twitF) (pePos ef)
tExpression (PE _ (Operation op ea eb)) = do
    lookup op >>= apply
    where
      lookup :: Typer (ProgElem String) Expr
      lookup (PE pos op) = gets (M.lookup op) >>= maybe (err ("Unknown operator: " ++ op) pos) return
      apply :: Typer Expr Expr
      apply (Expr (Func_ twitArgA (Func_ twitArgB twitResult)) f) =
        (\a b -> Expr twitResult $ Apply (Apply f a) b) <$> tExpressionAs twitArgA ea <*> tExpressionAs twitArgB eb
      apply (Expr twitF _) =
        err ("Operator expected, but found " ++ showTWit twitF) (pePos op)

--      Just Variable -> do
--        let var = VarRef $ Var twit ref
--        modify (insert ref (Expr twit var))
--        return $ TyperSuccess var

tExpressionAs :: forall a . TWit a -> Typer (ProgElem Expression) (LCExpr a)
tExpressionAs twitR e = tExpression e >>= as
    where
      as :: Typer Expr (LCExpr a)
      as (Expr twitV v) = case twitV .=. twitR of
        Just TEq -> return v
        Nothing  -> err ("Incorrect type: expected " ++ showTWit twitR ++ ", but found " ++ showTWit twitV) (pePos e)

tDefinitions :: Typer [ProgElem Definition] ()
-- Process definitions in reverse as a definition may reference any identifier defined after it.
-- TODO It would be good to report errors from all definitions, but this would require two parses 
--      or some dependency ordering.
tDefinitions = mapM_ tDefinition . reverse

tDefinition :: Typer (ProgElem Definition) ()
tDefinition (PE pos (Definition identifier e)) = do
    exp    <- tExpression e
    exists <- gets $ M.member identifier
    if exists
      then err (identifier ++ " is already defined") pos
      else modify $ M.insert identifier exp


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

typeProg :: Program -> TyperResult (LCExpr Picture)
typeProg = flip evalStateT prelude . tProgram 
