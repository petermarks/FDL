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

type TyperM = StateT Environment TyperResult

type Typer a b = a -> TyperM b

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
tExpression (PE pos (Reference ref)) = do
    meexpr <- gets (M.lookup ref)
    case meexpr of
      Nothing          -> err ("Unknown name: " ++ ref) pos
      Just Nothing     -> err ("Cannot determine type of " ++ ref ++ " from its context") pos
      Just (Just expr) -> return expr
tExpression (PE _ (Number num)) = expr . Prim . Const $ num
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
      lookup (PE pos op) = do
        meop <- gets (M.lookup op)
        case meop of
          Just (Just expr) -> return expr
          _                -> error $ "Unknown operator: " ++ op ++ " at " ++ show pos
      apply :: Typer Expr Expr
      apply (Expr (Func_ twitArgA (Func_ twitArgB twitResult)) f) =
        (\a b -> Expr twitResult $ Apply (Apply f a) b) <$> tExpressionAs twitArgA ea <*> tExpressionAs twitArgB eb
      apply (Expr twitF _) =
        err ("Operator expected, but found " ++ showTWit twitF) (pePos op)

tExpressionAs :: forall a . TWit a -> Typer (ProgElem Expression) (LCExpr a)
tExpressionAs twitR (PE pos (Reference ref)) = do
    meexpr <- gets (M.lookup ref)
    case meexpr of
      Nothing          -> err ("Unknown name: " ++ ref) pos
      Just Nothing     -> do
        let vr = VarRef $ Var twitR ref
        modify $ M.insert ref (Just $ Expr twitR vr)
        return vr
      Just (Just expr) -> as twitR pos expr
tExpressionAs twitR e = tExpression e >>= as twitR (pePos e)

as :: TWit a -> Pos -> Typer Expr (LCExpr a)
as twitR pos (Expr twitV v) = case twitV .=. twitR of
  Just TEq -> return v
  Nothing  -> err ("Incorrect type: expected " ++ showTWit twitR ++ ", but found " ++ showTWit twitV) pos

tDefinitions :: Typer [ProgElem Definition] ()
-- Process definitions in reverse as a definition may reference any identifier defined after it.
-- TODO It would be good to report errors from all definitions, but this would require two parses 
--      or some dependency ordering.
tDefinitions = mapM_ tDefinition . reverse

tDefinition :: Typer (ProgElem Definition) ()
tDefinition (PE pos (Definition identifier args e)) = do
    exp    <- foldr tLambda (tExpression e) args
    exists <- gets $ M.member identifier
    if exists
      then err (identifier ++ " is already defined") pos
      else modify $ M.insert identifier (Just exp)

tLambda :: ProgElem String -> Typer (TyperM Expr) Expr
tLambda (PE pos arg) e = do
    modify $ M.insert arg Nothing
    Expr twitBody body <- e
    argExp  <- gets (M.! arg)
    case argExp of
      Nothing                          -> err ("Cannot determine type of " ++ arg ++ " as it is not used") pos
      Just (Expr twitArg (VarRef var)) -> return $ Expr (Func_ twitArg twitBody) (Lambda var body)
      Just _                           -> error $ arg ++ " should be a VarRef"
      


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

typeProg :: Program -> TyperResult (LCExpr Picture)
typeProg = flip evalStateT prelude . tProgram 