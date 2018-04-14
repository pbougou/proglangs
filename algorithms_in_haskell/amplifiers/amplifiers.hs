{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = 
  do input <- BS.getContents
     let Just (_, r1) = readInt input
     let Just (m, r2) = readInt r1
     let (lst, _)     = readMany readInt r2
     let solution     = amplifiers lst m 0
     print (solution :: Int)
  where readInt s = BSC.readInt (BSC.dropWhile isSpace s)
        readMany readf s = case readf s of
          Just (x, r) -> let (xs, t) = readMany readf r
                         in  (x : xs, t)
          Nothing     -> ([], s)

cost :: Int -> Int -> Int -> Int
cost high low step = let diff = abs(high - low)
                         (q, r)  = diff `quotRem` step 
                      in if step >= diff && diff > 0 then 1 else q + if r > 0 then 1 else 0

amplifiers :: [Int] -> Int -> Int -> Int
amplifiers []           _ ans = ans
amplifiers [_]          _ ans = ans
amplifiers (x : y : xs) m ans = 
    if x <= y then amplifiers (y : xs) m (ans + cost y x m) 
              else let (decreasing, first, follow, ans'') = find_decreasing (x : y : xs) x [] m 0
                       ans' = (amplifiers decreasing m (ans + ans'')) + if first > x then (cost first x m) else 0
                    in if first >= 0 then amplifiers (first : follow) m ans' else amplifiers follow m ans'

-- until found in list larger than x
find_decreasing :: [Int] -> Int -> [Int] -> Int -> Int -> ([Int], Int, [Int], Int)
find_decreasing []           _   decr _    ans = (decr, -1, [], ans)
find_decreasing [x]          _   decr _    ans = (x : decr, x, [], ans)
find_decreasing (x : y : xs) mAx decr step ans = if x >= y then find_decreasing (y : xs) mAx (x : decr) step ans
                                                           else (if y >= mAx then (x : decr, y, xs, ans) else
                                                                let (decreasing, ans') = fix_decr (x : decr) y step 0 in 
                                                                    find_decreasing ((foldr (\z w -> w ++ [z]) [] decreasing) ++ xs) mAx [] step (ans + ans'))


fix_decr :: [Int] -> Int -> Int -> Int -> ([Int], Int)
fix_decr [x]          first step ans = ([first], ans + cost first x step)
fix_decr (x : y : xs) first step ans = if first < y then (first : y : xs, ans + cost first x step)
                                                           else if first == y then (first : xs, ans + cost first x step)
                                                                              else fix_decr (y : xs) first step (ans + cost y x step)



