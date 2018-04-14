import Data.Char
import System.IO

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readsPrec _ s =
    readParen True (\s ->
      [(Eabs x e, r)    |  ("\\", t1)  <-  lex s,
                           (x, t2)     <-  lex t1, isVar x,
                           (".", t3)   <-  lex t2,
                           (e, r)      <-  readsPrec 0 t3] ++
      [(Eapp e1 e2, r)  |  (e1, t)     <-  readsPrec 0 s,
                           (e2, r)     <-  readsPrec 0 t]) s ++
    [(Evar x, r) | (x, r) <- lex s, isVar x]
      where isVar x = isAlpha (head x) && all isAlphaNum x

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                putStrLn ("Parsed: " ++ show e)

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n readOne
