import Prelude hiding (lookup)
import Data.Char (isLower)
import Data.Map

type X  =  Char
data E  =  Lit X | Write E E | Read E | Var X | Lam X E | App E E

-- types
type Denot = State -> (Value, State)
type State = ([X], [X])
type Env   = Map X Value

-- datatypes: values as characters and functions
data Value = Val X | Fun (Value -> Denot)


-- semantic analysis for simple functional with IO lang
-- simple: lambda abstractions, applications, variables, constants
sem :: E -> Env -> Denot
sem expr env = case expr of
    Lit ch      -> \s -> (Val ch, s)
    Write el er -> \s -> sem er env $ snd $ (\s -> let eld = sem el env s in (fst eld, (fst $ snd eld, (snd $ snd eld) ++ [getValue $ fst eld]))) s
    Read e      -> \s -> 
        let 
             (h, t) = case fst s of 
                        [] -> error "End of input" 
                        _  -> (head $ fst s, tail $ fst s)
             d'     = sem e env (t, snd s) 
        in 
             case fst d' of
               Fun d -> d (Val h) (snd d')
               Val _ -> d'
    Var x       -> \s -> case lookup x env of
                            Just c  -> (c, s)
                            Nothing -> error "Unbound variable"
    Lam x e     -> case isLower x of
                      True  -> \s -> (Fun (\n -> sem e $ insert x n env), s)
                      False -> error "lexical error: Variables are letters"
    App el er   -> \s -> let (v', s') = sem er env s in 
                            case fst $ sem el env s' of
                              Fun d -> d v' s'
                              Val c -> sem el env s'

interpret :: Denot -> [X] -> IO()
interpret d input = putStrLn $ snd $ snd $ d (input, [])

main = do
    contents <- getContents
    let (phrase : input : _ ) = lines contents
    let e = (read phrase) :: E
    -- putStrLn $ show e
    let d = sem e empty
    interpret d input

-- Parsing expressions
next (x : r) = [(x, r)]
next _ = []

instance Read E where
  readsPrec p s =
    [(Lit c, r)        |  ('#', t) <- next s, (c, r) <- next t] ++
    [(Write e1 e2, r)  |  ('+', t1) <- next s,
                          (e1, t2) <- readsPrec 0 t1,
                          (e2, r) <- readsPrec 0 t2] ++
    [(Read e, r)       |  ('-', t) <- next s, (e, r) <- readsPrec 0 t] ++
    [(Var x, r)        |  (x, r) <- next s, isLower x] ++
    [(Lam x e, r)      |  ('/', t1) <- next s,
                          (x, t2) <- next t1, isLower x,
                          (e, r) <- readsPrec 0 t2] ++
    [(App e1 e2, r)    |  ('@', t1) <- next s,
                          (e1, t2) <- readsPrec 0 t1,
                          (e2, r) <- readsPrec 0 t2]


-- pretty printing of expressions
instance Show E where
  showsPrec p (Lit c) =
    ('#' :) . (c :)
  showsPrec p (Write e1 e2) =
    ('+' :) . showsPrec p e1 . showsPrec p e2
  showsPrec p (Read e) =
    ('-' :) . showsPrec p e
  showsPrec p (Var x) =
    (x :)
  showsPrec p (Lam x e) =
    ('/' :) . (x :) . showsPrec p e
  showsPrec p (App e1 e2) =
    ('@' :) . showsPrec p e1 . showsPrec p e2


getValue v = case v of
    Val x -> x
    Fun f -> error "I don't know how to print functions"


