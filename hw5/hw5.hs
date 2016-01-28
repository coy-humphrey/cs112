-- Necessary imports
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

-- Custom imports
import Data.List (intercalate) -- to clean up show
import Data.Maybe (fromJust, isNothing) -- to handle map maybes

--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op
               | For Variable Expression Expression Statement

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- P.identifier lexer
              P.reserved lexer "in"
              e1 <- exprParser
              P.reserved lexer "to"
              e2 <- exprParser
              P.reserved lexer "do"
              s <- stmtParser
              P.reserved lexer "end"
              return (For var e1 e2 s)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

--  Uncomment the following function for question #5 and #6

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing



-- Make AST nodes instances of Show
instance Show Value where
  show (IntVal i)  = show i
  show (BoolVal b) = if b then "true" else "false"

instance Show Op where
  show (Plus) = "+"
  show (Minus) = "-"
  show Times = "*"
  show GreaterThan = ">"
  show Equals = "="
  show LessThan = "<"

instance Show Expression where
  show (Var v) = v -- Variable is just a string
  show (Val v) = show v
  show (BinOp op e1 e2) =  intercalate " " [show e1, show op, show e2]
  show (Assignment v e) = intercalate " " [v, "=", show e]

instance Show Statement where
  show (Expr e) = show e
  show (If e s1 s2) = intercalate " " ["if", show e, "then", show s1, "else", show s2]
  show (While e s) = intercalate " " ["while", show e, "do", show s, "end"]
  show (Sequence s1 s2) = intercalate ";" [show s1, show s2] -- example showed no space in between
  show (Skip) = ""
  show (For v e1 e2 s) = intercalate " " ["for", v, "in", show e1, "to", show e2, "do", show s, "end"]

-- Implement evalE
evalE :: Expression -> Store -> (Value, Store)
evalE (BinOp o a b) s = (applyOp o a' b', s'')
    where (a', s')  = evalE a s
          (b', s'') = evalE b s'
evalE (Var x) s
    | isNothing v = error ("Variable " ++ x ++ "not found")
    | otherwise = (fromJust v, s)
    where v = Data.Map.lookup x s
evalE (Val v) s = (v, s)
evalE (Assignment x e) s = (v, s'')
    where (v, s') = evalE e s
          s'' = Data.Map.insert x v s'

-- Implement evalS
evalS :: Statement -> Store -> Store
evalS w@(While e s1) s =
  case (evalE e s) of
    (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
    (BoolVal False,s') -> s'
    _                  -> error "Condition must be a BoolVal"
evalS Skip s = s
evalS (Expr e) s = let (_, s') = evalE e s in s'
evalS (Sequence s1 s2) s =  let s'  = evalS s1 s
                                s'' = evalS s2 s'
                            in s''
evalS (If e s1 s2) s =
  case (evalE e s) of
    (BoolVal True,s')  -> let s'' = evalS s1 s' in s''
    (BoolVal False,s') -> let s'' = evalS s2 s' in s''
    _                  -> error "Condition must be a BoolVal"

evalS (For var e1 e2 st) s =
  -- Assign the value of e1 to var
  let (_,s') = evalE (Assignment var e1) s in
  -- Convert the for loop into a while loop
    evalS (While (BinOp LessThan (Var var) (BinOp Plus e2 (Val (IntVal 1))))
      (Sequence st (Expr(Assignment var (BinOp Plus (Var var) (Val (IntVal 1))))))) s'

evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s
  | isNothing v = Nothing
  | otherwise = Just (fromJust v, s)
  where v = Data.Map.lookup x s
evalE_maybe (Val v) s = Just (v, s)
evalE_maybe (Assignment x e) s
  | isNothing r = Nothing
  | otherwise = Just (v, s'')
  where r = evalE_maybe e s
        (v, s') = fromJust r
        s'' = Data.Map.insert x v s'

evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s = 
  case (evalE_maybe e s) of
    Nothing -> Nothing
    (Just res) -> case (res) of
      (BoolVal True, s')  -> case (evalS_maybe s1 s') of
        Nothing -> Nothing
        (Just s'') -> evalS_maybe w s''
      (BoolVal False, s') -> Just s'
      _                   -> Nothing

evalS_maybe Skip s = Just s
evalS_maybe (Expr e) s = 
  case (evalE_maybe e s) of
    Nothing -> Nothing
    (Just (_, s')) -> Just s'
evalS_maybe (Sequence s1 s2) s =
  case (evalS_maybe s1 s) of
    Nothing -> Nothing
    (Just s') -> case (evalS_maybe s2 s') of
      Nothing -> Nothing
      (Just s'') -> Just s''
evalS_maybe (If e s1 s2) s =
  case (evalE_maybe e s) of
    Nothing -> Nothing
    (Just res) -> case (res) of
      (BoolVal False, s') -> evalS_maybe s2 s'
      (BoolVal True, s') -> evalS_maybe s1 s'
      _                  -> Nothing

evalS_maybe (For var e1 e2 st) s =
  -- Assign the result of e1 to var
  case (evalE_maybe (Assignment var e1) s) of
    Nothing -> Nothing
    -- If that succeeds, convert the for loop into a while loop
    (Just (_,s')) -> evalS_maybe (While (BinOp LessThan (Var var) (BinOp Plus e2 (Val (IntVal 1))))
      (Sequence st (Expr(Assignment var (BinOp Plus (Var var) (Val (IntVal 1))))))) s'

newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)

getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))

evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do 
  a' <- evalE_monad a
  b' <- evalE_monad b
  return (applyOp o a' b')
evalE_monad (Var x) = do getVar x
evalE_monad (Val v) = do return v
evalE_monad (Assignment x e) = do 
  r <- evalE_monad e
  setVar x r

evalS_monad :: Statement -> Imperative ()
evalS_monad w@(While e s1) = do
  r <- evalE_monad e
  case (r) of
    (BoolVal True) -> do 
      evalS_monad s1
      evalS_monad w
    (BoolVal False) -> do return ()
evalS_monad Skip = do return ()
evalS_monad (Sequence s1 s2) = do
  evalS_monad s1
  evalS_monad s2
evalS_monad (Expr e) = do
  r <- evalE_monad e
  return ()
evalS_monad (If e s1 s2) = do
  r <- evalE_monad e
  case (r) of
    (BoolVal True) -> do evalS_monad s1
    (BoolVal False) -> do evalS_monad s2
evalS_monad (For var e1 e2 st) = do
  r1 <- evalE_monad e1
  -- initialize var
  setVar var r1
  -- Convert to a while loop
  -- While var < e2 + 1 (because e2 is inclusive)
  -- Do the first statement, then add 1 to var
  evalS_monad (While (BinOp LessThan (Var var) (BinOp Plus e2 (Val (IntVal 1))))
    (Sequence st (Expr(Assignment var (BinOp Plus (Var var) (Val (IntVal 1)))))))