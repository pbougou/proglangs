import ReadDensemWpp as R


type S = Var -> Integer

-- semantics for commands
semC :: C -> S -> S
semC Cskip         s  = s
semC (Cseq c1 c2)  s  = semC c2 (semC c1 s)
semC (Cexpr expr)  s  = snd $ semN expr s
semC (Cif b c1 c2) s = 
    let (b', s') = semB b s in
        case b' of
            True  -> semC c1 s'
            False -> semC c2 s'
semC (Cfor n c) s   = expon i (semC c) s'
  where (i, s') = semN n s
semC (Cwhile b c) s = fix bigF s
  where bigF f s = let (b', s') = semB b s in 
            case b' of
                True  -> f (semC c s')
                False -> s 

-- semantics for integer values
semN :: N -> S -> (Integer, S)
semN Nzero         s = (0, s)
semN (Nsucc n)     s = let sem = semN n s in (fst sem + 1, snd sem)
semN (Npred n)     s = let sem = semN n s in (fst sem - 1, snd sem)
semN (Nvar x)      s = (s x, s)
semN (Nassign x n) s = 
    let (val, state) = semN n s in
        (val, update state x val)
semN (Ninc x)      s = (s x, s')
    where s' = update s x ((s x) + 1) 
semN (Ndec x)      s = (s x, s')
    where s' = update s x ((s x) - 1) 

-- semantics for boolean values
semB :: B -> S -> (Bool, S)
semB Btrue       s = (True, s)
semB Bfalse      s = (False, s)
semB (Blt n1 n2) s = ((n1' < n2'), s'')
  where (n1', s')  = semN n1 s
        (n2', s'') = semN n2 s'
semB (Beq n1 n2) s = ((n1' == n2'), s'')
  where (n1', s')  = semN n1 s
        (n2', s'') = semN n2 s'
semB (Bnot b)    s = (not b', s')
  where (b', s') = semB b s

-- auxiliary functions
expon :: (Eq t, Num t) => t -> (b -> b) -> b -> b
expon 0 _ = id
expon n f = f . expon (n-1) f

update :: S -> Var -> Integer -> S--Var -> Integer  
update s x n y  | x == y    = n
                | otherwise = s y

s0 :: String -> a
s0 x = error ("not initialized variable " ++ x)

run :: C -> IO ()
run c = print $ semC c s0 "result"

main :: IO ()
main = do input <- getContents
          let c = read input :: C
          print c
          run c

fix :: (t -> t) -> t
fix f = f (fix f)
