Project 4: Set Calculator
=====================


> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Calc where


> import           Parsing2 hiding ((<|>))
> import qualified Data.Map as M
> import Text.Parsec (Parsec, ParseError, (<|>), try, parserFail)
> import qualified Text.Parsec as Parsec
> import Text.Parsec.String (Parser)
> import Text.Parsec.Expr
> import Text.Parsec.Token
> import Text.Parsec.Language (emptyDef)
> import qualified Control.Applicative as A
> import Data.List (intercalate, nub, sort, subsequences)
> import Data.Set (Set)
> import qualified Data.Set as S


-- Define the Expr type for the AST

> data Expr
>     = Set [Expr]
>     | Num Double
>     | Union Expr Expr
>     | Intersection Expr Expr
>     | Difference Expr Expr
>     | SymDiff Expr Expr
>     | Cartesian Expr Expr
>     | PowerSet Expr
>     | Complement Expr
>     | IsSubset Expr Expr
>     | IsMember Expr Expr
>     | Card Expr
>     deriving (Show, Eq)


-- Lexer setup

> lexer :: TokenParser ()
> lexer = makeTokenParser emptyDef


-- Parser for expressions

> parseexpr :: Parser Expr
> parseexpr = buildExpressionParser table parseterm
>     where
>         table = [ [ Infix (reservedOp lexer "union" >> return Union) AssocLeft
>          , Infix (reservedOp lexer "intersect" >> return Intersection) AssocLeft
>          , Infix (reservedOp lexer "minus" >> return Difference) AssocLeft
>          , Infix (reservedOp lexer "symdiff" >> return SymDiff) AssocLeft
>          , Infix (reservedOp lexer "cross" >> return Cartesian) AssocLeft
>          ]]


-- Parser for terms (numbers, sets, or parenthesized expressions)

> parseterm :: Parser Expr
> parseterm = parens lexer parseexpr
>         <|> try parsesetliteral
>         <|> (Num . either fromIntegral id <$> naturalOrFloat lexer)
>         <|> (PowerSet <$> (reserved lexer "P" >> parens lexer parseexpr))
>         <|> (Complement <$> (reservedOp lexer "'" >> parseterm))
>         <|> (IsSubset <$> (parseterm <* reservedOp lexer "⊆") <*> parseterm)
>         <|> (IsMember <$> (parseterm <* reservedOp lexer "∈") <*> parseterm)
>         <|> (Card <$> (reservedOp lexer "|" *> parseexpr <* reservedOp lexer "|"))


-- Parser for set literals like {1, 2, 3}

> parsesetliteral :: Parser Expr
> parsesetliteral = do
>     reservedOp lexer "{"
>     elements <- sepBy parseexpr (reservedOp lexer ",")
>     reservedOp lexer "}"
>     return $ Set elements


-- Evaluator for expressions

> eval :: Expr -> Either String (Set Double)
> eval (Num x) = Right $ S.singleton x
> eval (Set elements) = do
>     evaluated <- mapM eval elements
>     Right $ S.unions evaluated
> eval (Union a b) = S.union <$> eval a <*> eval b
> eval (Intersection a b) = S.intersection <$> eval a <*> eval b
> eval (Difference a b) = S.difference <$> eval a <*> eval b
> eval (SymDiff a b) = do
>     a' <- eval a
>     b' <- eval b
>     Right $ S.union (S.difference a' b') (S.difference b' a')
> eval (Cartesian a b) = do
>     a' <- eval a
>     b' <- eval b
>     -- Encode pairs as x*1000 + y to keep Set Double type
>     Right $ S.fromList [x * 1000 + y | x <- S.toList a', y <- S.toList b']
> eval (PowerSet a) = do
>     a' <- eval a
>     -- Return cardinalities of subsets instead of actual subsets
>     Right $ S.fromList [fromIntegral (length subset) | subset <- subsequences (S.toList a')]
> eval (Complement a) = do
>     a' <- eval a
>     Left "Complement operation requires a defined universe"
> eval (IsSubset a b) = do
>     a' <- eval a
>     b' <- eval b
>     Right $ if S.isSubsetOf a' b' then S.singleton 1 else S.singleton 0
> eval (IsMember x a) = do
>     x' <- eval x
>     a' <- eval a
>     case S.toList x' of
>         [val] -> Right $ if S.member val a' then S.singleton 1 else S.singleton 0
>         _ -> Left "Left operand of ∈ must be a single element"
> eval (Card a) = do
>     a' <- eval a
>     Right $ S.singleton $ fromIntegral $ S.size a'


-- Pretty-print expressions

> prettyprint :: Expr -> String
> prettyprint (Num x) = show x
> prettyprint (Set elements) = "{" ++ intercalate ", " (map prettyprint elements) ++ "}"
> prettyprint (Union a b) = prettyprint a ++ " ∪ " ++ prettyprint b
> prettyprint (Intersection a b) = prettyprint a ++ " ∩ " ++ prettyprint b
> prettyprint (Difference a b) = prettyprint a ++ " \\ " ++ prettyprint b
> prettyprint (SymDiff a b) = prettyprint a ++ " Δ " ++ prettyprint b
> prettyprint (Cartesian a b) = prettyprint a ++ " × " ++ prettyprint b
> prettyprint (PowerSet a) = "P(" ++ prettyprint a ++ ")"
> prettyprint (Complement a) = prettyprint a ++ "'"
> prettyprint (IsSubset a b) = prettyprint a ++ " ⊆ " ++ prettyprint b
> prettyprint (IsMember x a) = prettyprint x ++ " ∈ " ++ prettyprint a
> prettyprint (Card a) = "|" ++ prettyprint a ++ "|"


-- Format set for display

> formatSet :: Set Double -> String
> formatSet s = "{" ++ intercalate ", " (map show (S.toList s)) ++ "}"


-- Main calculator function

> calc :: String -> String
> calc input = case Parsec.parse parseexpr "" input of
>     Left err -> show err
>     Right expr -> prettyprint expr ++ "\n  = " ++ case eval expr of
>         Left err -> err
>         Right val -> formatSet val


-- Description shown at startup

> description :: String
> description = unlines
>   [ "Welcome to the Set Calculator!"
>   , "This calculator supports the following set operations:"
>   , "  - Set literals: {1, 2, 3}"
>   , "  - Union: A ∪ B"
>   , "  - Intersection: A ∩ B"
>   , "  - Difference: A \\ B"
>   , "  - Symmetric difference: A Δ B"
>   , "  - Cartesian product: A × B (encoded as x*1000 + y)"
>   , "  - Power set: P(A) (returns subset cardinalities)"
>   , "  - Complement: A' (requires universe)"
>   , "  - Subset test: A ⊆ B"
>   , "  - Membership test: x ∈ A"
>   , "  - Cardinality: |A|"
>   , "Type an expression, :help for more info, or :quit to exit."
>   ]


-- Help message

> helpMsg :: String
> helpMsg = unlines
>   [ "Set Calculator Help:"
>   , "  - Enter set expressions using the following operators:"
>   , "    ∪ : Union"
>   , "    ∩ : Intersection"
>   , "    \\ : Difference"
>   , "    Δ : Symmetric difference"
>   , "    × : Cartesian product (encoded as x*1000 + y)"
>   , "  - Use {} for set literals: {1, 2, 3}"
>   , "  - Other operations:"
>   , "    P(A) : Power set of A (returns subset sizes)"
>   , "    A' : Complement of A"
>   , "    A ⊆ B : Is A a subset of B?"
>   , "    x ∈ A : Is x an element of A?"
>   , "    |A| : Cardinality of A"
>   , "  - Example expressions:"
>   , "    {1, 2, 3} ∪ {3, 4, 5}"
>   , "    {1, 2} × {3, 4}  (results in {1003.0, 1004.0, 2003.0, 2004.0})"
>   , "    P({1, 2})"
>   , "    1 ∈ {1, 2, 3}"
>   , "    {1, 2} ⊆ {1, 2, 3}"
>   , "  - Type :quit to exit the calculator."
>   ]