module BirdLib where

import Tree
import System.Random (randomRIO)
import Data.Ratio


{- Helping functions for bird tree testing, q4.4 -}

-- https://en.wikipedia.org/wiki/Stern-Brocot_tree
-- ancestor_list :: Rational -> [Rational]

-- find integer and decimal part of a rational number
decimal_part :: Ratio Integer -> Rational -> (Integer, Rational)
decimal_part n n' = (int_part, (n' - fromInteger int_part) :: Rational)
    where int_part = (numerator n) `div` (denominator n)

-- continued sequence of decimal part 
cont_seq :: Integral a => Ratio a -> [a]
cont_seq n = cont_seq' (numerator n) (denominator n)
    where
        cont_seq' q r = if r /= 1 then q' : cont_seq' r r' else [q]
            where
                (q', r') = q `divMod` r

complete_cont_seq :: Ratio Integer -> [Integer]
complete_cont_seq n = if d /= 0 then ((fromInteger i) : cont_seq (1/d)) else [fromInteger i]
    where (i, d) = decimal_part n n

convert_seq_to_rational :: Fractional a => [a] -> a
convert_seq_to_rational lst = (1 / foldr (\x y -> x + 1/y) (head lst) (reverse $ tail lst)) -- :: Rational

complete_seq_to_rational :: Fractional a => [a] -> a
complete_seq_to_rational [x] = x
complete_seq_to_rational sq = tail_res  + (head sq)
    where tail_res = (convert_seq_to_rational $ (reverse (tail sq)))

parent sq = reverse (if (head (reverse sq)) - 1 > 0 then (head (reverse sq) - 1) : (tail (reverse sq)) else tail (reverse sq))

findBird q = if q == 1/2 || q == 2 then [1] else if q == 1 then [] else (q' : (findBird q'))
    where sq  = complete_cont_seq q
          sq' = parent sq
          q'  = complete_seq_to_rational (map fromInteger sq')

findBirdComplete q = reverse $ q : findBird q

-- Not used
isBird [y]       (T x _) = x == y && y == 1
isBird (x : xs) (T y ys) = if x == y then isBird' xs ys else False
    where
        isBird' _        []       = False
        isBird' []       _        = True
        isBird' (x : xs) (y : ys) = let T y' ys' = y in
            if x == y' then isBird' xs ys' else isBird' (x : xs) ys


convertBirds :: [Rational] -> Int -> [Int]
convertBirds [_]          _   = []
convertBirds (x : y : ys) cnt = if y > x && cnt `mod` 2 == 0      then (1 : convertBirds (y : ys) ((cnt + 1) `mod` 2)) 
                                else if y < x && cnt `mod` 2 == 0 then (0 : convertBirds (y : ys) ((cnt + 1) `mod` 2))
                                else if y > x && cnt `mod` 2 == 1 then (0 : convertBirds (y : ys) ((cnt + 1) `mod` 2)) 
                                else if y < x && cnt `mod` 2 == 1 then (1 : convertBirds (y : ys) ((cnt + 1) `mod` 2))
                                else []

-- generates random list with values 0 or 1
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (0,1)
  rs <- randomList (n-1)
  return (r:rs)

-- fibonacci numbers 
fibs = 1 : 2 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

