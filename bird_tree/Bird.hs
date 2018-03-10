import Tree
import BirdLib 
import System.IO.Unsafe
import Data.Ratio
import Test.QuickCheck

-- bstFind q (T x [l,r]) = if q == x then x else if q > x then bstFind q r else bstFind q r

-- Property for BirdLib testing...
prop_check_rationals :: Rational -> Bool
prop_check_rationals n = if n > 0 then (complete_seq_to_rational (map fromInteger $ complete_cont_seq n)) == n else True

-- 3. bird tree definition using mapTree from Tree module and mirror function
bird :: Tree Rational
bird = T 1 [mirror (mapTree (1 /) (mirror (mapTree (+1) bird))),  mirror (mapTree (\x -> 1/x +1) (mirror bird))]
    where
        mirror (T x [])     = T x []
        mirror (T x [l, r]) = T x [mirror r, mirror l]

-- 4. properties for testing in quickCheck
-- path in bird and in trimmed bird at height n are the same
prop_check_path :: NonNegative Int -> Bool
prop_check_path (NonNegative n) = if n >= 0 then path l bird  ==  path l (trimTree n bird) else True
    where
        l = unsafePerformIO (randomList n)
      
-- zig-zag function on bird tree produces all natural numbers
prop_naturals :: NonNegative Int -> Bool
prop_naturals (NonNegative n)  = if n >= 0 then pathList (take n l) bird == [1..n'] else True
    where
        n' = (fromIntegral n + 1):: Rational
        l  = 1 : 0 : l

-- the denominators of nodes of the most left path are successive fibonacci numbers
prop_fibonacci :: Positive Int -> Bool
prop_fibonacci (Positive n) = if n >= 1 then pathList (take n (repeat 0)) (mapTree denominator bird) == take (n+1) fibs else True

-- all rational numbers exist in bird tree
prop_rationals_in_bird :: (Positive Rational) -> Bool
prop_rationals_in_bird (Positive n) = if n > 0 then n == path lst bird' else True
    where 
        lst = convertBirds (findBirdComplete n) 0
        hgt = length lst
        bird' = trimTree hgt bird

main = do
    putStrLn ""
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_check_path
    putStrLn ""
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_naturals
    putStrLn ""
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_fibonacci
    putStrLn ""
    quickCheckWith stdArgs { maxSuccess = 100 } prop_rationals_in_bird
    
