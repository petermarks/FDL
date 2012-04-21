module Graphics.FDL.Typer
    ( TyperError(..)
    , TyperResult(..)
    , typeProg
    ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad.State
import Data.Traversable
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

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
tProgram (Program e ds) =
    tDefinitions (tExpressionAs Picture_ e) ds

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

tDefinitions :: TyperM (LCExpr a) -> Typer [ProgElem Definition] (LCExpr a)
-- Sort definitions by dependency before typing.
-- TODO It would be good to report errors from all definitions, but that is tricky because of dependencies.
tDefinitions body defs = sortDefs >>= foldr tDefinition body
    where
      sortDefs = 
          check
        . map (right (M.fromList (map (getIdentifier &&& id) defs) M.!))
        . topologicalSort
        . M.fromList
        . map (getIdentifier &&& findDeps)
        $ defs
      getIdentifier (PE _ (Definition i _ _)) = i
      findDeps (PE _ (Definition _ as e)) = findExprDeps e S.\\ S.fromList (map peElem as)
      -- TODO use uniplate os some other SYB
      findExprDeps (PE _ (Reference     s  )) = S.singleton s
      findExprDeps (PE _ (Number        _  )) = S.empty
      findExprDeps (PE _ (Pairing       l r)) = findExprDeps l `S.union` findExprDeps r
      findExprDeps (PE _ (Application   f v)) = findExprDeps f `S.union` findExprDeps v
      findExprDeps (PE _ (Operation   _ l r)) = findExprDeps l `S.union` findExprDeps r
      check = traverse (circErr ||| return)
      circErr identifiers = err ("Circular definitions: " ++ intercalate ", " identifiers) (pePos . fromJust $ find (\(PE _ (Definition i _ _)) -> i == head identifiers) defs)

tDefinition :: ProgElem Definition -> TyperM (LCExpr a) -> TyperM (LCExpr a)
tDefinition (PE pos (Definition identifier args e)) body = do
    Expr twit exp <- isolate $ foldr tLambda (tExpression e) args
    exists        <- gets $ M.member identifier
    when exists $ err (identifier ++ " is already defined") pos
    let var = Var twit identifier
    modify $ M.insert identifier . Just . Expr twit . VarRef $ var
    bodyExp <- body
    return $ Apply (Lambda var bodyExp) exp

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


----------------------------------------------------------------------
-- Library
----------------------------------------------------------------------

topologicalSort :: Ord a => M.Map a (S.Set a) -> [Either [a] a]
topologicalSort graph 
    | M.null graph              = []
    | (x, _) <- M.findMin graph = process [] x topologicalSort graph
    where
      process stack x cont graph
        | x `elem` stack = 
          Left (x : takeWhile (/= x) stack) : cont graph
        | (Just deps, graph') <- lookupDelete x graph = 
          S.fold (process (x : stack)) ((Right x :) . cont) deps graph'
        | otherwise =
          cont graph

lookupDelete :: Ord k => k -> M.Map k a -> (Maybe a, M.Map k a)
lookupDelete = M.updateLookupWithKey (\_ _ -> Nothing)

-- | Ensure that no state changes escape - run the given action then restore the state to its previous value
isolate :: (MonadState s m) => m a -> m a
isolate m = do
  s <- get
  r <- m
  put s
  return r

