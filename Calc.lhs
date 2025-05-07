> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Calc where
> 
> import           Parsing2 hiding ((<|>))
> import           Text.Parsec           (Parsec, ParseError, (<|>), try, parserFail, sepBy, many1)
> import           Text.Parsec.Char      (char, letter, alphaNum, noneOf)
> import qualified Text.Parsec           as Parsec
> import           Text.Parsec.String    (Parser)
> import           Text.Parsec.Expr      (buildExpressionParser, Assoc(..), Operator(..))
> import           Text.Parsec.Token     (makeTokenParser, GenLanguageDef(..), LanguageDef, TokenParser, reserved, reservedOp, parens, naturalOrFloat)
> import           Text.Parsec.Language  (emptyDef)
> import           Data.Char             (isAlpha, isSpace)
> import           Data.List             (intercalate, nub, sort, subsequences)
> import           Data.Set              (Set)
> import qualified Data.Set              as S
> 
> -- Lexer definition
> myDef :: LanguageDef st
> myDef = emptyDef
>     { reservedNames   = ["card","powerset","subset","in"]
>     , reservedOpNames = ["union","intersect","minus","symdiff","cross","'" ]
>     , identStart      = letter
>     , identLetter     = alphaNum <|> char '_'
>     }
> 
> lexer :: TokenParser st
> lexer = makeTokenParser myDef
> 
> -- AST
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
> -- Parser for set literals
> parsesetliteral :: Parser Expr
> parsesetliteral = do
>     reservedOp lexer "{"
>     exprs <- (reservedOp lexer "}" >> return [])
>              <|> sepBy parseexpr (reservedOp lexer ",") <* reservedOp lexer "}"
>     return $ Set exprs
> 
> -- Parser for multi-letter elements
> parseChar :: Parser Expr
> parseChar = do
>     s <- between (char '\'') (char '\'') (many1 (noneOf "'"))
>     return $ Elem s
> 
> -- Parser for terms
> parseterm :: Parser Expr
> parseterm =
>       parens lexer parseexpr
>   <|> try parsesetliteral
>   <|> try parseChar
>   <|> (Num . either fromIntegral id <$> naturalOrFloat lexer)
> 
> -- Expression parser with operators
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
> -- Evaluator definitions
> eval :: Expr -> Either String Expr
> eval (Set xs)          = Set <$> mapM eval xs
> eval (Num x)           = Right $ Num x
> eval (Elem s)          = Right $ Elem s
> eval (Union a b)       = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set xs, Set ys) -> Right $ Set (nub (sort (xs ++ ys)))
>         (Set xs, y)      -> Right $ Set (nub (sort (xs ++ [y])))
>         (x, Set ys)      -> Right $ Set (nub (sort (x:ys)))
>         (x, y)           -> Right $ Set (nub (sort [x, y]))
>
> eval (Intersection a b)= do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set xs, Set ys) -> Right $ Set (filter (`elem` ys) xs)
>         (Set xs, y)      -> Right $ Set (filter (== y) xs)
>         (x, Set ys)      -> Right $ Set (filter (== x) ys)
>         _                -> Left "Intersection expects two sets"
>
> eval (Difference a b)  = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set xs, Set ys) -> Right $ Set (filter (`notElem` ys) xs)
>         (Set xs, y)      -> Right $ Set (filter (/= y) xs)
>         (x, Set ys)      -> Right $ if x `elem` ys then Set [] else x
>         _                -> Left "Difference expects two sets"
>
> eval (SymDiff a b)     = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set xs, Set ys) -> Right $ Set (filter (`notElem` ys) xs ++ filter (`notElem` xs) ys)
>         (Set xs, y)      -> Right $ Set (filter (/= y) xs ++ if y `elem` xs then [] else [y])
>         (x, Set ys)      -> Right $ Set (if x `elem` ys then filter (/= x) ys else x : filter (/= x) ys)
>         _                -> Left "SymDiff expects two sets"
>
> eval (Cartesian a b)   = do
>     a' <- eval a
>     b' <- eval b
>     case (a', b') of
>         (Set xs, Set ys) -> Right $ Set [Set [x,y] | x <- xs, y <- ys]
>         _                -> Left "Cartesian expects two sets"
>
> eval (PowerSet a)      = do
>     v <- eval a
>     case v of
>         Set xs -> Right $ Set (map Set (subsequences xs))
>         _      -> Left "PowerSet expects a set"
>
> eval (Complement _)    = Left "Complement requires universe definition"
>
> eval (IsSubset a b)    = do x <- eval a; y <- eval b; Right $ IsSubset x y
> eval (IsMember x a)    = do v <- eval x; s <- eval a; Right $ IsMember v s
>
> eval (Card a)          = do
>     v <- eval a
>     case v of
>         Set xs -> Right $ Num (fromIntegral (S.size (S.fromList xs)))
>         _      -> Right $ Num 1.0
>
> -- Pretty-printing
> prettyprint :: Expr -> String
> prettyprint (IsSubset (Set xs) (Set ys))
>     | all (`elem` ys) xs = "true"
>     | otherwise          = "false"
> prettyprint (IsMember x (Set ys))
>     | x `elem` ys        = "true"
>     | otherwise          = "false"
> prettyprint (Num x)
>     | x == fromIntegral (round x) = show (round x)
>     | otherwise                    = show x
> prettyprint (Elem s)    = "'" ++ s ++ "'"
> prettyprint (Set es)    = "{" ++ intercalate ", " (map prettyprint es) ++ "}"
> prettyprint (Card e)    = prettyprint (Num $ case eval e of Right (Num n) -> n; _ -> 0)
> prettyprint expr        = case expr of
>     Union a b        -> prettyprint a ++ " union " ++ prettyprint b
>     Intersection a b -> prettyprint a ++ " intersect " ++ prettyprint b
>     Difference a b   -> prettyprint a ++ " minus " ++ prettyprint b
>     SymDiff a b      -> prettyprint a ++ " symdiff " ++ prettyprint b
>     Cartesian a b    -> prettyprint a ++ " cross " ++ prettyprint b
>     PowerSet a       -> "powerset(" ++ prettyprint a ++ ")"
>     Complement a     -> prettyprint a ++ "'"
>     _                -> "error"
> 
> -- Main REPL function with simple assignment
> calc :: String -> String
> calc input =
>     let t = dropWhile isSpace input
>     in case break (=='=') t of
>         (v,'=':rest)
>           | not (null v)
>           , all (\c->isAlpha c||c=='_') (filter (not.isSpace) v)
>           -> calc rest
>         _ -> case Parsec.parse parseexpr "" t of
>               Left err -> show err
>               Right ex -> either id prettyprint (eval ex)
> 
> -- Description shown at startup
> description :: String
> description = unlines
>   [ "Set Calculator REPL"
>   , "Type set expressions to evaluate them"
>   , "Available commands:"  
>   , "  :help   - Show this help message"
>   , "  :quit   - Exit the calculator"
>   ]
> 
> -- Help message
> helpMsg :: String
> helpMsg = unlines
>   [ "SET CALCULATOR HELP"
>   , "BASIC OPERATIONS:"  
>   , "  A union B      - Elements in either A or B"
>   , "  A intersect B  - Elements in both A and B"
>   , "  A minus B      - Elements in A but not in B"
>   , "  A symdiff B    - Elements in either A or B but not both"
>   , "  A cross B      - Cartesian product of A and B"
>   , "SET FUNCTIONS:"  
>   , "  card(A)        - Number of unique elements in A"
>   , "  powerset(A)    - All possible subsets of A"
>   , "RELATIONAL OPERATIONS:"  
>   , "  A subset B     - true if all elements of A are in B"
>   , "  x in A         - true if x is an element of A"
>   , "SPECIAL CASES:"  
>   , "  {}             - Empty set"
>   , "EXAMPLES:"  
>   , "  card({1,2,2,3}) = 3"
>   , "  {1,2} union {2,3} = {1,2,3}"
>   , "  {1,2} subset {1,2,3} = true"
>   ]
