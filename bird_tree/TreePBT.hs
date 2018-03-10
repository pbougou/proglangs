--module TreePBT where

import Tree
import Control.Monad
import Text.Show.Functions
import Test.QuickCheck

-- 1. TODO resize
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree
    where
      arbTree :: Arbitrary a => Int -> Gen (Tree a)
      arbTree 0 = do
        a <- arbitrary
        return $ T a []
      arbTree n = do
        (Positive m) <- arbitrary
        let n' = n `div` (m + 1)
        f <- replicateM m (arbTree n')
        a <- arbitrary
        return $ T a f

-- 2.

-- 2.1
prop_size_height :: Tree Int -> Bool
prop_size_height t = (sizetree t) >= (heightTree t)

-- 2.2
prop_max_intree :: Tree Int -> Bool
prop_max_intree t = inTree (maxtree t) t

-- 2.3
prop_nodes_intree :: Tree Int -> Bool
prop_nodes_intree t = foldr (\x y -> (inTree x t) && y) True (nodes t)

-- 2.4
prop_countf_size :: (Int -> Bool) -> Tree Int -> Bool
prop_countf_size f t = countTree f t <= sizetree t

-- 2.5
prop_nodes_leaves_size :: Tree Int -> Bool
prop_nodes_leaves_size t = length (nodes t) == sizetree t && (length (leaves t) < sizetree t || length (leaves t) == 1 && sizetree t == 1)

-- 2.6
prop_maptree_sz_height :: (Int -> Int) -> Tree Int -> Bool
prop_maptree_sz_height f t = sizetree (mapTree f t) == sizetree t && heightTree (mapTree f t) == heightTree t

-- 2.7
prop_maptree_n :: (Int -> Int) -> Int -> Tree Int -> Property
prop_maptree_n f n t = inTree n t ==> inTree (f n) (mapTree f t)

-- 2.8
prop_nodes_leaves_map :: (Int -> Int) -> Tree Int -> Bool
prop_nodes_leaves_map f t = (map f . nodes $ t) == (nodes . mapTree f) t && (map f . leaves $ t) == (leaves . mapTree f) t


main = do
    putStrLn "Testing tree module..."
    putStrLn "Size is always greater or equal than height"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_size_height
    putStrLn "Max value of nodes exists in the tree"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_max_intree
    putStrLn "All nodes exist in the tree"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nodes_intree
    putStrLn "All nodes that satisfy f are less or equal than size of tree"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_countf_size
    putStrLn "Length of nodes is equal to size and leaves are less than #nodes or both are equal to 1"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nodes_leaves_size
    putStrLn "MapTree does not affect size or height"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_maptree_sz_height
    putStrLn "If n exists in t, then f n exists in maptree f t"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_maptree_n
    putStrLn "map f . g = g . mapTree f, where g = {nodes, leaves}"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nodes_leaves_map






