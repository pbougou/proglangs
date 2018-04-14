{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (fromJust)
import Control.Monad (replicateM)
import Data.Map (Map, lookup, empty, insert)
import qualified Data.Map as Map
import Prelude hiding (lookup)

{-
 - Defining our type system: Simply typed lambda calculus:
 -   variables, lamba abstractions, applications    
 --}

data E    = Var String   | Lam String E   | App E E deriving (Eq, Show)

{-
 - types: 
 -      variables and transitions(take input sigma and produce output taf).
 --}
data Type = TyVar Int    | TyTrans Type Type 
    deriving Eq
 
data A    = Empty | Annovar Type | Annolam A Type | Annoapp A A Type deriving (Eq, Show) 

isVariable (c : cs) = isAlpha c && (foldr (&&) (True) (map isAlphaNum cs))

instance Read E where
  readsPrec p s = readParen True  ( \s ->  [(Lam x e, r) |  ("\\", t1) <- lex s,
                                                            (x, t2)    <- lex t1, isVariable x,
                                                            (".",t3)   <- lex t2,
                                                            (e, r)     <- readsPrec 0 t3] ++
                                           [(App e1 e2, r) |  (e1, t2) <- readsPrec 0 s,
                                                              (e2, r)  <- readsPrec 0 t2]) s ++
                                           [(Var x, r) | (x, r) <- lex s, isVariable x]

instance Show Type where
  showsPrec p (TyVar a)     = ("@" ++) . (showsPrec 0 a)
  showsPrec p (TyTrans m n) = showParen (p == 1) $ (showsPrec 1 m) . ( " -> " ++) . (showsPrec 0 n)


-- L_{untyped} -> L_{typed}
gae :: E -> State (Int, Map String Int) A 
gae expr = 
    case expr of
        Var v   -> do (id, gSet) <- get
                      case Map.lookup v gSet of
                         Just t  -> return $ Annovar (TyVar t)
                         Nothing -> error "Wrong input: unbound variable"
        Lam v e -> do (id, gSet) <- get
                      modify (\s -> (id + 1, Map.insert v id gSet))
                      e' <- gae e
                      return $ Annolam e' (TyTrans (TyVar id) (getannotype e'))
        App m n -> do (id, gSet) <- get
                      modify (\s -> (id + 1, gSet))
                      m' <- gae m
                      modify (\s -> (fst s, gSet))
                      n' <- gae n
                      return $ Annoapp m' n' (TyVar id)

getannoexpr :: E -> A
getannoexpr expr = evalState (gae expr) (0, Map.empty)


constraints annoexpr = mkConstraints annoexpr []
    where
        mkConstraints annoexpr listofit 
            | annoexpr == Empty = 
                case listofit of 
                  [] -> []
                  xs -> let (x : xst) = xs in mkConstraints x xst
            | otherwise = 
                let (x : xst) = listofit in 
                   case annoexpr of
                     Annovar _     -> if listofit == [] then [] else mkConstraints x xst
                     Annolam e _   -> mkConstraints e listofit
                     Annoapp m n t -> let (sigma, taf) = (getannotype m, getannotype n) in
                                       (sigma, TyTrans taf t) : (mkConstraints m (n : listofit))



unify [] types                                             = Just types
unify ((t1,t2) : c) types | t1 == t2                       = unify c types
unify ((TyVar t1, t2) : c) types | notInType (TyVar t1) t2 = unify c' types'
   where v      = (TyVar t1, t2) 
         c'     = map (tplofTwo . subs $ v) c
         types' = subs v types
unify ((t1, TyVar t2) : c) types | notInType (TyVar t2) t1 = unify c' types'
   where v      = (TyVar t2, t1)  
         c'     = map (tplofTwo . subs $ v) c
         types' = subs v types
unify ((TyTrans t11 t12, TyTrans t21 t22) : c) types       = unify c' types
   where t1 = (t11, t21)
         t2 = (t12, t22)
         c' = t1 : t2 : c
unify _                                        _           = Nothing


lo :: Type -> State (Int, Map Int Int) Type
lo t = 
    case t of 
        TyVar v -> 
            do (id, map) <- get
               case Map.lookup v map of
                    Nothing -> do modify (\s -> (id + 1, Map.insert v id map))
                                  return $ TyVar id
                    Just k  -> do return $ TyVar k
        TyTrans l r -> do l' <- lo l
                          r' <- lo r
                          return $ TyTrans l' r'
                    

beSure t = case t of
    Just t  -> 
        putStrLn $ show $ lexicOrder t
          where
            lexicOrder :: Type -> Type
            lexicOrder t = evalState (lo t) (0, Map.empty)
    Nothing -> putStrLn "type error"


lineProcess = do  s <- getLine
                  let expr     = (read s) :: E
                  let annoexpr = getannoexpr expr
                  let ty       = unify (constraints annoexpr) (getannotype annoexpr)
                  beSure ty

main = do  n  <- readLn
           replicateM n lineProcess
    
getannotype :: A -> Type
getannotype t = case t of  
                  (Annovar t')     -> t'
                  (Annolam _ t')   -> t'
                  (Annoapp _ _ t') -> t' 

subs (t1, t2) (TyVar t)       = if t1 == TyVar t then t2 else TyVar t
subs (t1, t2) (TyTrans s taf) = TyTrans t1' t2'
      where t1' = subs (t1, t2) s
            t2' = subs (t1, t2) taf

notInType t1 (TyVar t2)    = t1 /= TyVar t2
notInType t1 (TyTrans s t) = t1 /= TyTrans s t && notInType t1 s && notInType t1 t

tplofTwo f (x,y) = (f x, f y)
