module Tree where

data Tree a = T a [Tree a]
    deriving Show

-- 1.
foldtree :: (a -> [b] -> b) -> Tree a -> b
foldtree f (T x xs) = f x (map (foldtree f) xs)


-- 2.
sizetree :: Num b => Tree a -> b
sizetree t = foldtree (\x ys -> 1 + sum ys) t

heightTree :: (Ord b, Num b) => Tree a -> b
heightTree t = foldtree (\_ ys -> 1 + foldr (\x y -> if x > y then x else y) 0 ys) t

sumtree :: Num a => Tree a -> a
sumtree t = foldtree (\x ys -> x + sum ys) t

maxtree :: Ord a => Tree a -> a
maxtree t = foldtree (\x ys -> (foldr (\x y -> if x > y then x else y) x ys)) t

inTree :: Eq a => a -> Tree a -> Bool
inTree x t = foldtree (\y ys -> (foldr (\w z -> w || z) (x == y) ys)) t

nodes :: Tree a -> [a]
nodes t = foldtree (\y ys -> ((concat . concat) ([[y]] : [ys]))) t

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f t = foldtree (\x xs -> foldr (\w z -> z + w) (if f x then 1 else 0) xs) t

-- Converts a tree of a nodes to a tree of (nodes of a, number of children)
convert :: Tree a -> Tree (a, Int)
convert (T x []) = (T (x,0) [])
convert (T x ys) = (T (x, (length ys)) ys') 
    where
     convert' lst = map convert lst 
     ys' = convert' ys

leaves t = foldtree (\x y -> if y == [] then [x] else concat y) t
-- TODO write leaves without convert
{-
leaves :: Tree a -> [a]
leaves t = foldtree (\w z -> (foldr (\x y -> x ++ y) (if (snd w) == 0 then [fst w] else []) z)) t'
    where 
        t' = convert t
-}
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f t = foldtree (\y ys -> T (f y) ys) t


-- 3.
trimTree :: Int -> Tree a -> Tree a
trimTree n (T x xs) = if n == 0 then (T x []) else (T x xs')
    where
        xs' = trimTree' (n-1) xs
        trimTree' n ts = map (trimTree n) ts


path :: [Int] -> Tree a -> a
path []       (T x _)  = x
path (x : xs) (T _ ys) = path xs ((!!) ys x)

-- needed for bird tree testing
pathList :: [Int] -> Tree a -> [a]
pathList []       (T x _)  = [x]
pathList (x : xs) (T y ys) = y : (pathList xs ((!!) ys x))

-- Input
tx = T 1 [ T 2 []
          , T 7 [ T 4 []
                , T 5 [ T 18 [
                                T 69 []
                             ], 
                        T 46 [T 1821 [ T 19 []],
                              T 20 []
                             ]
                      ]
                ]
          , T 6 []
          , T 42 []
          , T 17 []
         ]

