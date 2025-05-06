> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Calc where
> 
> import           Parsing2 hiding ((<|>))
> import qualified Data.Map              as M
> import           Text.Parsec           (Parsec, ParseError, (<|>), try, parserFail, sepBy, many1)
> import           Text.Parsec.Char      (char, letter, alphaNum)
> import qualified Text.Parsec           as Parsec
> import           Text.Parsec.String    (Parser)
> import           Text.Parsec.Expr      
> import           Text.Parsec.Token     (makeTokenParser, GenLanguageDef(..), LanguageDef, TokenParser, reserved, reservedOp, parens, naturalOrFloat)
> import           Text.Parsec.Language  (emptyDef)
> import qualified Control.Applicative   as A
> import           Data.List             (intercalate, nub, sort, subsequences)
> import           Data.Char             (isAlpha)
> import           Data.Set              (Set)
> import qualified Data.Set              as S
> 
> -- Language definition for lexer
> myDef :: LanguageDef st
> myDef = emptyDef
>     { reservedNames   = ["card","powerset","subset","in"]
>     , reservedOpNames = ["union","intersect","minus","symdiff","cross","'"]
>     , identStart      = letter
>     , identLetter     = alphaNum <|> char '_'
>     }
> 
> lexer :: TokenParser st
> lexer = makeTokenParser myDef
> 
> -- Define the Expr type for the AST
> data Expr
>     = Set [Expr]
>     | Num Double
>     | Elem String
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
> 
> -- Parser for set literals (including empty set)
> parsesetliteral :: Parser Expr
> parsesetliteral = do
>     reservedOp lexer "{"
>     body <- (reservedOp lexer "}" >> return (Set []))
>          <|> do
>              elements <- sepBy parseexpr (reservedOp lexer ",")
>              reservedOp lexer "}"
>              return (Set elements)
>     return body
> 
> -- Parser for expressions with proper operator precedence
> parseexpr :: Parser Expr
> parseexpr = buildExpressionParser table parseterm
>   where
>     table =
>       [ [ Prefix (reserved lexer "card"     >> return Card)
>         , Prefix (reserved lexer "powerset" >> return PowerSet)
>         ]
>       , [ Postfix (reservedOp lexer "'"     >> return Complement) ]
>       , [ Infix  (reservedOp lexer "cross"  >> return Cartesian)    AssocLeft ]
>       , [ Infix  (reservedOp lexer "union"    >> return Union)        AssocLeft
>         , Infix  (reservedOp lexer "intersect" >> return Intersection) AssocLeft
>         , Infix  (reservedOp lexer "minus"     >> return Difference)   AssocLeft
>         , Infix  (reservedOp lexer "symdiff"   >> return SymDiff)      AssocLeft
>         ]
>       , [ Infix  (reserved lexer "subset"   >> return IsSubset)     AssocNone
>         , Infix  (reserved lexer "in"       >> return IsMember)     AssocNone
>         ]
>       ]
> 
> -- Parser for "atoms" only: braces, chars, numbers
> parseterm :: Parser Expr
> parseterm =
>     parens lexer parseexpr
>     <|> try parsesetliteral
>     <|> try parseChar
>     <|> (Num . either fromIntegral id <$> naturalOrFloat lexer)
> 
> parseChar :: Parser Expr
> parseChar = do
>     s <- between (char '\'') (char '\'') (many1 letter)
>     return $ Elem s
> 
> -- Evaluator
> eval :: Expr -> Either String Expr
> eval (Set [])        = Right $ Set []
> eval (Num x)         = Right $ Num x
> eval (Elem s)       = Right $ Elem s
> eval (Set elements)  = Set <$> mapM eval elements
> 
> eval (Union a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], Set []) -> Right $ Set []
>         (Set [], y)      -> Right y
>         (x, Set [])      -> Right x
>         (Set xs, Set ys) -> Right $ Set (nub (sort (xs ++ ys)))
>         (Set xs, y)      -> Right $ Set (nub (sort (xs ++ [y])))
>         (x, Set ys)      -> Right $ Set (nub (sort (x:ys)))
>         (x, y)           -> Right $ Set (nub (sort [x, y]))
> 
> eval (Intersection a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _)       -> Right $ Set []
>         (_, Set [])       -> Right $ Set []
>         (Set xs, Set ys)  -> Right $ Set (filter (`elem` ys) xs)
>         (Set xs, y)       -> Right $ Set (filter (== y) xs)
>         (x, Set ys)       -> Right $ Set (filter (== x) ys)
>         (x, y)            -> Right $ if x == y then x else Set []
> 
> eval (Difference a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _)       -> Right $ Set []
>         (x, Set [])       -> Right x
>         (Set xs, Set ys)  -> Right $ Set (filter (`notElem` ys) xs)
>         (Set xs, y)       -> Right $ Set (filter (/= y) xs)
>         (x, Set ys)       -> Right $ if x `elem` ys then Set [] else x
>         (x, y)            -> Right $ if x == y then Set [] else x
> 
> eval (SymDiff a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], Set [])  -> Right $ Set []
>         (Set [], y)       -> Right y
>         (x, Set [])       -> Right x
>         (Set xs, Set ys)  -> Right $ Set (filter (`notElem` ys) xs ++ filter (`notElem` xs) ys)
>         (Set xs, y)       -> Right $ Set (filter (/= y) xs ++ if y `elem` xs then [] else [y])
>         (x, Set ys)       -> Right $ Set (if x `elem` ys then filter (/= x) ys else x : filter (/= x) ys)
>         (x, y)            -> Right $ if x == y then Set [] else Set [x, y]
> 
> eval (Cartesian a b) = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set [], _)       -> Right $ Set []
>         (_, Set [])       -> Right $ Set []
>         (Set xs, Set ys)  -> Right $ Set [Set [x, y] | x <- xs, y <- ys]
>         (Set xs, y)       -> Right $ Set [Set [x, y] | x <- xs]
>         (x, Set ys)       -> Right $ Set [Set [x, y] | y <- ys]
>         (x, y)            -> Right $ Set [Set [x, y]]
> 
> eval (PowerSet a) = do
>     a' <- eval a
>     case a' of
>         Set []            -> Right $ Set [Set []]
>         Set xs            -> Right $ Set [Set ys | ys <- subsequences xs]
>         x                 -> Right $ Set [Set [], Set [x]]
> 
> eval (Complement _) = Left "Complement requires universe definition"
> 
> eval (IsSubset a b) = do
>     a' <- eval a
>     b' <- eval b
>     Right $ IsSubset a' b'
>
> eval (IsMember x a) = do
>     x' <- eval x
>     a' <- eval a
>     Right $ IsMember x' a'
>
> eval (Card a) = do
>     a' <- eval a
>     case a' of
>         Set xs            ->
>             let cnt = S.size (S.fromList xs)
>             in  Right $ Num (fromIntegral cnt)
>         _                  ->
>             Right $ Num 1.0
> 
> -- Pretty-printing
> prettyprint :: Expr -> String
> prettyprint (Num x)
>     | x == fromIntegral (round x) = show (round x)
>     | otherwise                    = show x
> prettyprint (Elem s)           = "'" ++ s ++ "'"
> prettyprint (Set [])              = "{}"
> prettyprint (Set elements)        = "{" ++ intercalate ", " (map prettyprint elements) ++ "}"    
> prettyprint (Union a b)           = prettyprint a ++ " union " ++ prettyprint b
> prettyprint (Intersection a b)    = prettyprint a ++ " intersect " ++ prettyprint b
> prettyprint (Difference a b)      = prettyprint a ++ " minus " ++ prettyprint b
> prettyprint (SymDiff a b)         = prettyprint a ++ " symdiff " ++ prettyprint b
> prettyprint (Cartesian a b)       = prettyprint a ++ " cross " ++ prettyprint b
> prettyprint (PowerSet a)          = "powerset(" ++ prettyprint a ++ ")"
> prettyprint (Complement a)        = prettyprint a ++ "'"
> prettyprint (IsSubset a b)        = 
>     case eval (IsSubset a b) of
>         Right (Num 1) -> "true"
>         Right (Num 0) -> "false"
>         _             -> "error"
> prettyprint (IsMember x a)        = 
>     case eval (IsMember x a) of
>         Right (Num 1) -> "true"
>         Right (Num 0) -> "false"
>         _             -> "error"
> prettyprint (Card a)              = 
>     case eval (Card a) of
>         Right (Num n) -> show (round n)
>         _             -> "error"
> 
> -- Main calculator function
> calc :: String -> String
> calc input = case Parsec.parse parseexpr "" input of
>     Left err                       -> show err
>     Right expr                     -> case eval expr of
>         Left err1                  -> err1
>         Right val                  -> prettyprint val
> 
> -- Description shown at startup
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
> 
> -- Comprehensive help message
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
