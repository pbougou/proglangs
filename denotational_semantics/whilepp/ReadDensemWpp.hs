module ReadDensemWpp where

import Data.Char
import Text.Read
import Text.Read.Lex

-- Syntax

type Var = String

data C = Cskip | Cexpr N | Cseq C C | Cfor N C | Cif B C C | Cwhile B C 
    deriving Show
data N = Nzero | Nsucc N | Npred N | Nvar Var
               | Nassign Var N | Ninc Var | Ndec Var
    deriving Show
data B = Btrue | Bfalse | Blt N N | Beq N N | Bnot B
    deriving Show
-- Parsing
isVar :: [Char] -> Bool
isVar x = all isAlpha x && not (x `elem` keywords)
  where keywords = ["zero", "succ", "true", "not", "skip",
                    "for", "if", "then", "else", "while", "do"]

when :: Monad m => Bool -> m a -> m a
when True p = p
when False _ = fail "when failed"

instance Read N where
  readPrec = parens $
             (prec 2 $ do
                Number n <- lexP
                when (numberToInteger n == Just 0) $ do
                  return Nzero) <++
             (prec 1 $ do
                Ident "succ" <- lexP
                n <- readPrec
                return (Nsucc n)) <++
             (prec 1 $ do
                Ident "pred" <- lexP
                n <- readPrec
                return (Npred n)) <++
             (prec 0 $ do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol ":=" <- lexP
                  n <- readPrec
                  return (Nassign x n)) <++
             (prec 2 $ do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol "++" <- lexP
                  return (Ninc x)) <++
             (prec 2 $ do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol "--" <- lexP
                  return (Ndec x)) <++
             (prec 2 $ do
                Ident x <- lexP
                when (isVar x) $ do
                  return (Nvar x)) 

instance Read B where
  readPrec = parens $
             (prec 2 $ do
                Ident "true" <- lexP
                return Btrue) <++
             (prec 2 $ do
                Ident "false" <- lexP
                return Bfalse) <++
             (prec 0 $ do
                n1 <- readPrec
                Symbol "<" <- lexP
                n2 <- readPrec
                return (Blt n1 n2)) <++
             (prec 0 $ do
                Ident "not" <- lexP
                b <- readPrec
                return (Bnot b)) <++
             (prec 0 $ do
                n1 <- readPrec
                Symbol "==" <- lexP
                n2 <- readPrec
                return (Beq n1 n2))

instance Read C where
  readPrec = parens $
             (prec 0 $ do
                c1 <- step readPrec
                Punc ";" <- lexP
                c2 <- readPrec
                return (Cseq c1 c2)) <++
             (prec 1 $ do
                Ident "skip" <- lexP
                return Cskip) <++
             (prec 1 $ do
                n <- reset readPrec
                return (Cexpr n)) <++
             (prec 1 $ do
                Ident "if" <- lexP
                b <- reset readPrec
                Ident "then" <- lexP
                c1 <- readPrec
                Ident "else" <- lexP
                c2 <- readPrec
                return (Cif b c1 c2)) <++
             (prec 1 $ do
                Ident "for" <- lexP
                n <- reset readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cfor n c)) <++
             (prec 1 $ do
                Ident "while" <- lexP
                b <- reset readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cwhile b c))                

-- Pretty-printing
{-
instance Show N where
  showsPrec p Nzero = ("0" ++)
  showsPrec p (Nsucc n) = ("succ " ++) . showsPrec 1 n
  showsPrec p (Npred n) = ("pred " ++) . showsPrec 1 n
  showsPrec p (Nvar x) = (x ++)
  showsPrec p (Nassign x n) =
    showParen (p > 0) $
    (x ++) . (" := " ++) . showsPrec 0 n
  showsPrec p (Ninc x) = (x ++) . ("++" ++)
  showsPrec p (Ndec x) = (x ++) . ("--" ++)

instance Show B where
  showsPrec p Btrue = ("true" ++)
  showsPrec p Bfalse = ("false" ++)
  showsPrec p (Blt n1 n2) = showsPrec 0 n1 . (" < " ++) . showsPrec 0 n2
  showsPrec p (Beq n1 n2) = showsPrec 0 n1 . (" = " ++) . showsPrec 0 n2
  showsPrec p (Bnot b) = ("not " ++) . showsPrec 0 b

instance Show C where
  showsPrec p Cskip = ("skip" ++)
  showsPrec p (Cexpr n) = showsPrec 0 n
  showsPrec p (Cseq c1 c2) =
    showParen (p > 0) $
    showsPrec 1 c1 . ("; " ++) . showsPrec 0 c2
  showsPrec p (Cfor n c) =
    ("for " ++) . showsPrec 0 n . (" do " ++) . showsPrec 1 c
  showsPrec p (Cif b c1 c2) =
    ("if " ++) . showsPrec 0 b . (" then " ++) . showsPrec 1 c1 .
                                 (" else " ++) . showsPrec 1 c2
  showsPrec p (Cwhile b c) =
    ("while " ++) . showsPrec 0 b . (" do " ++) . showsPrec 1 c
-}

-- Main function: parsing a statement and pretty-printing
{-
main = do  input <- getContents
           let c :: C
               c = read input
           print c

-}
