Project 2: Strict Set Calculator
=====================

> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}

> module Calc where

> import           Parsing2 hiding ((<|>))
> import qualified Data.Map as M
> import Text.Parsec (Parsec, ParseError, (<|>), try, parserFail, char, letter)
> import qualified Text.Parsec as Parsec
> import Text.Parsec.String (Parser)
> import Text.Parsec.Expr
> import Text.Parsec.Token
> import Text.Parsec.Language (emptyDef)
> import qualified Control.Applicative as A
> import Data.List (intercalate, nub, sort, subsequences)
> import Data.Set (Set)
> import qualified Data.Set as S
> import Data.Char (isAlpha)

-- Define the Expr type for the AST

> data Expr
>     = Set [Expr]
>     | Num Double
>     | CharElem Char
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
>     deriving (Show, Eq, Ord)

-- Lexer setup

> lexer :: TokenParser ()
> lexer = makeTokenParser emptyDef

-- Parser for set literals (including empty set)

> parsesetliteral :: Parser Expr
> parsesetliteral = do
>     reservedOp lexer "{"
>     (do reservedOp lexer "}" >> return (Set [])
>      <|> do elements <- sepBy parseexpr (reservedOp lexer ",")
>             reservedOp lexer "}"
>             return (Set elements))

-- Parser for expressions

> parseexpr :: Parser Expr
> parseexpr = buildExpressionParser table parseterm
>     where
>         table = [ [ Infix (reservedOp lexer "union" >> return Union) AssocLeft
>                  , Infix (reservedOp lexer "intersect" >> return Intersection) AssocLeft
>                  , Infix (reservedOp lexer "minus" >> return Difference) AssocLeft
>                  , Infix (reservedOp lexer "symdiff" >> return SymDiff) AssocLeft
>                  , Infix (reservedOp lexer "cross" >> return Cartesian) AssocLeft
>                  ]]

-- Parser for terms

> parseterm :: Parser Expr
> parseterm = parens lexer parseexpr
>         <|> try parsesetliteral
>         <|> try parseChar
>         <|> (Num . either fromIntegral id <$> naturalOrFloat lexer)
>         <|> (PowerSet <$> (reserved lexer "powerset" >> parens lexer parseexpr))
>         <|> (Complement <$> (reservedOp lexer "'" >> parseterm))
>         <|> (IsSubset <$> (parseterm <* reservedOp lexer "subset") <*> parseterm)
>         <|> (IsMember <$> (parseterm <* reservedOp lexer "in") <*> parseterm)
>         <|> (Card <$> (reserved lexer "card" >> parens lexer parseexpr))

> parseChar :: Parser Expr
> parseChar = do
>     c <- between (char '\'') (char '\'') letter
>     return $ CharElem c

-- Evaluator

> eval :: Expr -> Either String Expr
> eval (Set []) = Right $ Set []
> eval (Num x) = Right $ Num x
> eval (CharElem c) = Right $ CharElem c
> eval (Set elements) = do
>     evaluated <- mapM eval elements
>     Right $ Set evaluated

> eval (Union a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], Set []) -> Right $ Set []
>         (Set [], y) -> Right y
>         (x, Set []) -> Right x
>         (Set xs, Set ys) -> Right $ Set (nub (sort (xs ++ ys)))
>         (Set xs, y) -> Right $ Set (nub (sort (xs ++ [y])))
>         (x, Set ys) -> Right $ Set (nub (sort (x:ys)))
>         (x, y) -> Right $ Set (nub (sort [x, y]))

> eval (Intersection a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _) -> Right $ Set []
>         (_, Set []) -> Right $ Set []
>         (Set xs, Set ys) -> Right $ Set (filter (`elem` ys) xs)
>         (Set xs, y) -> Right $ Set (filter (== y) xs)
>         (x, Set ys) -> Right $ Set (filter (== x) ys)
>         (x, y) -> Right $ if x == y then x else Set []

> eval (Difference a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _) -> Right $ Set []
>         (x, Set []) -> Right x
>         (Set xs, Set ys) -> Right $ Set (filter (`notElem` ys) xs)
>         (Set xs, y) -> Right $ Set (filter (/= y) xs)
>         (x, Set ys) -> Right $ if x `elem` ys then Set [] else x
>         (x, y) -> Right $ if x == y then Set [] else x

> eval (SymDiff a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], Set []) -> Right $ Set []
>         (Set [], y) -> Right y
>         (x, Set []) -> Right x
>         (Set xs, Set ys) -> Right $ Set (filter (`notElem` ys) xs ++ filter (`notElem` xs) ys)
>         (Set xs, y) -> Right $ Set (filter (/= y) xs ++ if y `elem` xs then [] else [y])
>         (x, Set ys) -> Right $ Set (if x `elem` ys then filter (/= x) ys else x : filter (/= x) ys)
>         (x, y) -> Right $ if x == y then Set [] else Set [x, y]

> eval (Cartesian a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _) -> Right $ Set []
>         (_, Set []) -> Right $ Set []
>         (Set xs, Set ys) -> Right $ Set [Set [x, y] | x <- xs, y <- ys]
>         (Set xs, y) -> Right $ Set [Set [x, y] | x <- xs]
>         (x, Set ys) -> Right $ Set [Set [x, y] | y <- ys]
>         (x, y) -> Right $ Set [Set [x, y]]

> eval (PowerSet a) = do
>     a' <- eval a
>     case a' of
>         Set [] -> Right $ Set [Set []]
>         Set xs -> Right $ Set [Set ys | ys <- subsequences xs]
>         x -> Right $ Set [Set [], Set [x]]

> eval (Complement _) = Left "Complement requires universe definition"

> eval (IsSubset a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _) -> Right (Num 1)
>         (_, Set []) -> case a' of
>                         Set [] -> Right (Num 1)
>                         _ -> Right (Num 0)
>         (Set xs, Set ys) -> Right (Num (if all (\x -> x `elem` ys) xs then 1 else 0))
>         (x, Set ys) -> Right (Num (if x `elem` ys then 1 else 0))
>         _ -> Right (Num 0)

> eval (IsMember x a) = do
>     x' <- eval x
>     a' <- eval a
>     case a' of
>         Set [] -> Right (Num 0)
>         Set ys -> Right (Num (if x' `elem` ys then 1 else 0))
>         y -> Right (Num (if x' == y then 1 else 0))

> eval (Card a) = do
>     a' <- eval a
>     case a' of
>         Set xs -> do
>             evaluated <- mapM eval xs
>             let unique = nub evaluated
>             return $ Num (fromIntegral (length unique) :: Double)
>         _ -> Right $ Num 1.0

-- Pretty-printing

> prettyprint :: Expr -> String
> prettyprint (Num x) 
>     | x == fromIntegral (round x) = show (round x)
>     | otherwise = show x
> prettyprint (CharElem c) = "'" ++ [c] ++ "'"
> prettyprint (Set []) = "{}"
> prettyprint (Set elements) = "{" ++ intercalate ", " (map prettyprint elements) ++ "}"
> prettyprint (Union a b) = prettyprint a ++ " union " ++ prettyprint b
> prettyprint (Intersection a b) = prettyprint a ++ " intersect " ++ prettyprint b
> prettyprint (Difference a b) = prettyprint a ++ " minus " ++ prettyprint b
> prettyprint (SymDiff a b) = prettyprint a ++ " symdiff " ++ prettyprint b
> prettyprint (Cartesian a b) = prettyprint a ++ " cross " ++ prettyprint b
> prettyprint (PowerSet a) = "powerset(" ++ prettyprint a ++ ")"
> prettyprint (Complement a) = prettyprint a ++ "'"
> prettyprint (IsSubset a b) = 
>     case eval (IsSubset a b) of
>         Right (Num 1) -> "true"
>         Right (Num 0) -> "false"
>         _ -> "error"
> prettyprint (IsMember x a) = 
>     case eval (IsMember x a) of
>         Right (Num 1) -> "true"
>         Right (Num 0) -> "false"
>         _ -> "error"
> prettyprint (Card a) = 
>     case eval (Card a) of
>         Right (Num n) -> show (floor n)  
>         _ -> "error"

-- Main calculator function

> calc :: String -> String
> calc input = case Parsec.parse parseexpr "" input of
>     Left err -> show err
>     Right expr -> case eval expr of
>         Left err -> err
>         Right val -> prettyprint val

-- Description shown at startup

> description :: String
> description = unlines
>   [ "Set Calculator REPL"
>   , "Type set expressions to evaluate them"
>   , "Available commands:"
>   , "  :help   - Show this help message"
>   , "  :quit   - Exit the calculator"
>   , ""
>   , "Examples:"
>   , "  {1,2} union {2,3}    - Set union"
>   , "  {1,2} intersect {2,3} - Set intersection"
>   , "  card({1,2,2})        - Count unique elements (returns 2)"
>   , "  {1,2} subset {1,2,3} - Subset check (returns true)"
>   ]

-- Comprehensive help message

> helpMsg :: String
> helpMsg = unlines
>   [ "SET CALCULATOR HELP"
>   , ""
>   , "BASIC OPERATIONS:"
>   , "  A union B      - Elements in either A or B"
>   , "  A intersect B  - Elements in both A and B"
>   , "  A minus B      - Elements in A but not in B"
>   , "  A symdiff B    - Elements in either A or B but not both"
>   , "  A cross B      - Cartesian product of A and B"
>   , ""
>   , "SET FUNCTIONS:"
>   , "  card(A)        - Number of unique elements in A"
>   , "  powerset(A)    - All possible subsets of A"
>   , "  A'             - Complement of A (requires universe)"
>   , ""
>   , "RELATIONAL OPERATIONS:"
>   , "  A subset B     - true if all elements of A are in B"
>   , "  x in A         - true if x is an element of A"
>   , ""
>   , "SPECIAL CASES:"
>   , "  {}             - Empty set"
>   , "  {1,2,3}       - Set containing elements 1, 2, and 3"
>   , "  {{1,2},3}      - Set containing a nested set {1,2} and element 3"
>   , ""
>   , "EXAMPLES:"
>   , "  card({1,2,2,3})          = 3"
>   , "  {1,2} union {2,3}        = {1,2,3}"
>   , "  {1,2} intersect {2,3}    = {2}"
>   , "  {1,2,3} subset {1,2,3,4} = true"
>   , "  powerset({1,2})          = {{},{1},{2},{1,2}}"
>   , ""
>   , "Type :quit to exit the calculator"
>   ]