module Graphics.FDL.Parser
    ( Pos(..)
    , Error(..)
    , Program(..)
    , ProgElem(..)
    , Expression(..)
    , Definition(..)
    , parseProg
    , parseWith
    ) where

import Data.Char
import Data.Traversable
import Text.ParserCombinators.UU hiding (Parser)

----------------------------------------------------------------------
-- Parser state and instances
----------------------------------------------------------------------

data Pos = Pos
    { posChar :: Int
    , posLine :: Int
    , posCol  :: Int
    }
    deriving (Show, Eq)

startPos :: Pos
startPos = Pos 0 1 1

instance IsLocationUpdatedBy Pos Char where
    advance (Pos char line col) c = case c of
      '\n' -> Pos (char + 1) (line + 1) 1
      _    -> Pos (char + 1) line       (col + 1)

instance IsLocationUpdatedBy Pos String where
    advance  = foldl advance 

type Parser a = P (Str Char Pos) a 


----------------------------------------------------------------------
-- Untyped AST
----------------------------------------------------------------------

data Program = Program (ProgElem Expression) [ProgElem Definition]
    deriving (Show)

data ProgElem a = PE
    { pePos  :: Pos
    , peElem :: a
    }

instance (Show a) => Show (ProgElem a) where
    showsPrec d (PE _ a) = showsPrec d a

data Expression
    = Reference   String
    | Number      Double
    | Pairing     (ProgElem Expression) (ProgElem Expression)
    | Application (ProgElem Expression) (ProgElem Expression)
    | Operation   (ProgElem String) (ProgElem Expression) (ProgElem Expression)
    -- | Lambda      String Expression
    deriving (Show)

data Definition = Definition String (ProgElem Expression)
    deriving (Show)


----------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------

pProgram :: Parser Program
pProgram = Program <$> (pList (pAnySym " \t\n") *> pLines 0) <*> pDefinitions <* pList (pAnySym " \t\n")

pLines :: Int -> Parser (ProgElem Expression)
pLines indent = pChainr_ng (apply2PE . Operation <$> pPE (";" <$ pNewline <* pIndent indent)) (pLine indent)

pLine :: Int -> Parser (ProgElem Expression)
pLine indent = pInline `optPostfix_ng` (apply2PE Application <$$> pIndented indent)

pIndented :: Int -> Parser (ProgElem Expression)
pIndented parentIndent = pNewline *> pWS >>= (\l -> if l > parentIndent then pLines l else empty) . length

pInline :: Parser (ProgElem Expression)
pInline = foldr pChainl_ng pParticle (map pOperator operators)

-- Lowest precedence to highest
operators :: [[String]]
operators =
    [ ["+", "-"]
    , ["*", "/"]
    ]

pOperator :: [String] -> Parser (ProgElem Expression -> ProgElem Expression -> ProgElem Expression)
pOperator = pAny $ \op -> pWS *> (apply2PE . Operation <$> pPE (pSeqSym op)) <* pWS

pParticle :: Parser (ProgElem Expression)
pParticle = pChainl_ng (apply2PE Application <$ pWS) pAtomic

pAtomic :: Parser (ProgElem Expression)
pAtomic = pReference <<|> pNumber <<|> pBracketed

pReference :: Parser (ProgElem Expression)
pReference = pPE $ Reference <$> pIdentifier

pIdentifier :: Parser String
pIdentifier = (:) <$> pSym ('a','z') <*> pMunch isAlphaNum

pNumber :: Parser (ProgElem Expression)
pNumber = pPE $ Number . read <$> (((:) <$> pSign) `opt_ng` id <*> pDigits <??> ((++) <$$> pDecimal))
    where
      pSign    = pSym '-' `micro` 1 -- cost so that (2-1) is parsed as subtraction
      pDigits  = pList1 $ pSym ('0','9')
      pDecimal = (:) <$> pSym '.' <*> pDigits

pBracketed :: Parser (ProgElem Expression)
pBracketed =
  pRepos <*> (pSym '(' *> pWS *> pInline `optPostfix_ng` pPair <* pWS <* pSym ')')

pPair :: Parser (ProgElem Expression -> ProgElem Expression)
pPair = apply2PE Pairing <$$> (pWS *> pSym ',' *> pWS *> pInline)

pDefinitions :: Parser [ProgElem Definition]
pDefinitions = pList_ng (pNewline *> pDefinition)

pDefinition :: Parser (ProgElem Definition)
pDefinition = pPE $ Definition <$> (pIdentifier <* pWS <* pSym '=') <*> pIndented 0

pWS :: Parser String
pWS = pList (pAnySym " \t")

pNewline :: Parser ()
pNewline = () <$ pList1_ng (pWS *> pSym '\n')

pIndent :: Int -> Parser ()
pIndent indent = do
    ws <- pWS
    if length ws == indent then return () else empty

pPE :: Parser a -> Parser (ProgElem a)
pPE p = PE <$> pPos <*> p

pRepos :: Parser (ProgElem a -> ProgElem a)
pRepos = (\pos (PE _ x) -> PE pos x) <$> pPos

apply2PE :: (ProgElem a -> ProgElem b -> c) -> ProgElem a -> ProgElem b -> ProgElem c
apply2PE f a@(PE pos _) b = PE pos $ f a b

pSeqSym :: Provides st s s => [s] -> P st [s]
pSeqSym = sequenceA . map pSym

opt_ng ::  P st a -> a -> P st a
p `opt_ng` v = must_be_non_empty "opt_ng" p (p <|> pure v) 

optPostfix_ng :: P st a -> P st (a -> a) -> P st a
p `optPostfix_ng` q = must_be_non_empty "optPostfix_ng" q (p <**> (q `opt_ng` id))


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

parseProg :: String -> (Program, [Error Pos])
parseProg input = parse ((,) <$> pProgram <*> pEnd) (Str input [] startPos True)

parseWith :: Parser a -> String -> (a, [Error Pos])
parseWith p input = parse ((,) <$> p <*> pEnd) (Str input [] startPos True)

