import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad
import Data.Char
------------------------
-- Backend
------------------------

-- Expression
data Exp = Constant Int
        | Variable String
        | Minus Exp Exp
        | Plus Exp Exp
        | Greater Exp Exp
        | Less Exp Exp
        | Equal Exp Exp
        | Times Exp Exp
        | Div Exp Exp
        deriving Show
        
-- Commands
data Com = Assign String Exp
        | Seq Com Com
        | Cond Exp Com Com
        | While Exp Com
        | Declare String Exp Com
        | Print Exp
        deriving Show
        
s1 = Declare "x" (Constant 150)
        (Declare "y" (Constant 200)
            (Seq (While (Greater (Variable "x") (Constant 0))
                    (Seq (Assign "x" (Minus (Variable "x") (Constant 1)))
                        (Assign "y" (Minus (Variable "y") (Constant 1)))
                    )
                )
                (Print (Variable "y"))
            )
        )

-- data structures of the environment
-- the variables will be stored in a list
type Stack = [Int]
-- Location of a variable would be its position in the list
type Location = Int
-- Index of a variable is the symbols table
type Index = [String]

-- get location from the name
-- remember: the location is just an Int
position :: String -> Index -> Location
position name index = let 
                        pos n (nm:nms) = if name == nm
                                        then n
                                        else pos (n+1) nms
                        in pos 1 index  -- the index starts from 1

-- fetch value from the location
fetch :: Location -> Stack -> Int
fetch n (v:vs) = if n == 1 then v else fetch (n-1) vs


-- put is to substitute the variable in the corresponding positionS
put :: Location -> Int -> Stack -> Stack
put n x (v:vs) = if n == 1
                -- if the replacement of the head is required
                then x : vs
                else v : (put (n-1) x vs)


-- select a monad to be used as a model of calculus instead of a virtual machine
-- a is the return value
newtype M a = StOut (Stack -> (a, Stack, String))

instance Monad M where
    return x = StOut (\n -> (x, n, ""))
    e >>=  f = StOut (\n -> let (a, n1, s1) = (unStOut e) n
                                (b, n2, s2) = unStOut (f a) n1
                            in (b, n2, s1++s2))

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure  = return
  (<*>) = ap

unStOut (StOut f) = f

getfrom :: Location -> M Int
getfrom i = StOut (\ns -> (fetch i ns, ns, ""))

write :: Location -> Int -> M()
write i v = StOut(\ns -> ((), put i v ns, ""))

push :: Int -> M ()
push x = StOut(\ns -> ((), x:ns, ""))

pop :: M ()
pop = StOut (\m -> let (n:ns) = m
                    in ((), ns, ""))
                    
eval1 :: Exp -> Index -> M Int
eval1 exp index = case exp of 
                    Constant n -> return n
                    Variable x -> let loc = position x index
                                  in getfrom loc
                    Minus x y -> do { a <- eval1 x index;
                                      b <- eval1 y index;
                                      return (a-b); }
                    Plus x y -> do { a <- eval1 x index;
                                      b <- eval1 y index;
                                      return (a+b); }
                    Greater x y -> do { a <- eval1 x index;
                                        b <- eval1 y index;
                                        return (if a > b
                                                then 1
                                                else 0) }
                    Times x y -> do { a <- eval1 x index;
                                      b <- eval1 y index;
                                      return (a*b); }

interpret1 :: Com -> Index -> M()
interpret1 stmt index = case stmt of
                Assign name e -> let loc = position name index
                                 in do { v <- eval1 e index;
                                         write loc v }
                Seq s1 s2 -> do { x <- interpret1 s1 index;
                                  y <- interpret1 s2 index;
                                  return () }
                Cond e s1 s2 -> do { x <- eval1 e index;
                                     if x == 1
                                     then interpret1 s1 index
                                     else interpret1 s2 index }
                While e b -> let loop () = do { v <- eval1 e index;
                                                if v == 0 then return ()
                                                else do { interpret1 b index; 
                                                          loop() } }
                             in loop ()
                Declare nm e stmt -> do { v <- eval1 e index;
                                          push v;
                                          interpret1 stmt (nm:index);
                                          pop }
                Print e -> do { v <- eval1 e index;
                                output v }
                                
output :: Show a => a -> M ()
output v = StOut (\n -> ((), n, show v))

test a = unStOut (eval1 a[]) []
interp a = unStOut (interpret1 a []) []

-------------------------
-- Frontend
-------------------------

newtype Parser a = Parser(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
    -- return :: a -> Parser a
    return a = Parser (\cs -> [(a, cs)])
    -- bind :: Parser a -> (a -> Parser b) -> Parser bS
    p >>= f = Parser (\cs -> concat [ parse (f a) cs' 
                                        | (a, cs') <- parse p cs] )

item :: Parser Char
item = Parser (\xs -> case xs of 
                        "" -> []
                        (c:cs) -> [(c, cs)])

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

-- when have multiple choices, choose the first one
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of 
                            [] -> []
                            (x:xs) -> [x])

-- condition
sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item;
             if p c then return c else mzero }

-- syntax sugar for general version of sat
(?) :: Parser a -> (a-> Bool) -> Parser a
p ? test = do { b <- p;
                if test b then return b else mzero}

char :: Char -> Parser Char
char c = sat (c == )

string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c;
                     string cs;
                     return (c:cs) }


-- many
many0 :: Parser a -> Parser [a]
many0 p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p;
               as <- many0 p;
               return (a:as) }
--
-- lexical combinator
--

space :: Parser String
space = many0 (sat isSpace)

-- token is followed by space 
token :: Parser a -> Parser a
token p = do { a <- p;
               space;
               return a }

-- symbol is used to parse a specific keyword
symbol :: String -> Parser String
symbol cs = token (string cs)

-- discard the starting space
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do { space; p })

-- identifier
ident :: Parser [Char]
ident = do { l <- sat isAlpha;
             lsc <- many0 (sat (\a -> isAlpha a || isDigit a));
             return (l:lsc) }

-- ... and may be followed by spaces
identif :: Parser [Char]
identif = token ident

-- Parser for variables
var :: Parser Exp
var = do { v <- identif;
           return (Variable v) }

-- ???
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
                    where
                        rest a = (do { f <- op;
                                       b <- p;
                                       rest (f a b) })
                                 +++ return a


-- Paser for one and more than one digits
digit :: Parser Exp
digit = do { x <- token (sat isDigit);
             return (Constant (ord x - ord '0'))}

-- unsigned number
digiti :: Parser Exp
digiti = do { p <- digit;
              l <- many digit;
              return (foldl (\a b -> let Constant nra = a
                                         Constant nrb = b
                                     in Constant (10*nra + nrb))
                            (Constant 0)
                            (p:l)) }

-- Parser for expressions
rexp :: Parser Exp
rexp = expr `chainl1` relop

expr :: Parser Exp
expr = term `chainl1` addop

term :: Parser Exp
term = factor `chainl1` mulop

-- Parser for factor, where factor :: var | digiti | (expr)
factor = var +++
         digiti +++
         do { symbol "(";
              n <- rexp;
              symbol ")";
              return n }

-- Parser for operators are using the data-constructors
addop :: Parser (Exp -> Exp -> Exp)
addop = do { symbol "-";
             return (Minus) }
        +++
        do { symbol "+";
             return (Plus) }

mulop :: Parser (Exp -> Exp -> Exp)
mulop = do { symbol "*"; 
             return (Times) }
        +++
        do { symbol "/"; 
             return (Div) }

relop :: Parser (Exp -> Exp -> Exp)
relop = do { symbol ">"; 
             return (Greater) }
        +++
        do { symbol "<"; 
             return (Less) }
        +++
        do { symbol "="; 
             return (Equal) }

-- print
printe :: Parser Com
printe = do { symbol "print"; 
              x <- rexp; 
              return (Print x) }

-- assign
assign :: Parser Com
assign = do{ x <-identif;
             symbol ":="; e <- rexp;
             return (Assign x e) }

-- seq
seqv :: Parser Com
seqv = do { symbol "{" ; 
            c <- com; 
            symbol ";" ; 
            d <- com; 
            symbol "}";
            return (Seq c d) }

-- cond
cond :: Parser Com
cond = do { symbol "if" ; 
            e <- rexp;
            symbol "then" ; 
            c <- com;
            symbol "else" ; 
            d <- com;
            return (Cond e c d) }

-- while
while :: Parser Com
while = do { symbol "while";
             e <- rexp;
             symbol "do";
             c <- com;
             return (While e c) }

-- declare
declare :: Parser Com
declare = do { symbol "declare";
               x <- identif;
               symbol "=";
               e <- rexp;
               symbol "in" ;
               c <- com;
               return (Declare x e c ) }

-- com
com :: Parser Com
com = assign +++ 
      seqv +++ 
      cond +++ 
      while +++ 
      declare +++ 
      printe

code :: String
code = "declare x = 150 in \
        \    declare y = 200 in \
        \        {while x > 0 do { x:=x-1; \
        \            y:=y-1 \
        \        }; \
        \    print y \
        \}"

-----------------------
-- Combine
-----------------------
unparse :: [(Com, String)] -> Com
unparse x = fst (head x)

run = \s -> interp (unparse (parse com s))