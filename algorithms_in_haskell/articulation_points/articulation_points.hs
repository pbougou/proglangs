{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Data.Maybe (fromJust)
import Data.Array
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

--------------------------------------------------------------------------------------------------
mkGraph :: Ix a => (a, a) -> [(a, a)] -> Array a [a]
mkGraph bnds es =
        accumArray (\xs x -> x:xs) [] bnds
                    ([(x1,x2) | (x1,x2) <- es] ++
                     [(x2,x1) | (x1,x2)<-es,x1/=x2])

mkSum n = foldr (+) 0 [1..n]
-------------------------------------------------------------------------------------------------------
aux :: Integer -> Integer -> [Integer] -> Set Integer -> Map Integer Integer 
       -> Integer -> (Maybe Integer, [Integer], Integer)
aux i p_i []       _   dep l = (Nothing, [], l)
aux i p_i (c : cs) vis dep l = if Set.notMember c vis then (Just c, c : cs, l) else aux i p_i cs vis dep l'
    where
          Just d_ni = Map.lookup c dep
          l'        = if c /= p_i then l `min` d_ni else l



getArtPoints _    _ _ _     _   _   _   _ 0    ch res = (ch, res)
     
getArtPoints True i d depth low par vis g expl ch res = getArtPoints is ni d' depth' low' par' vis' g' expl'  ch' res
                where
-- First case (init phase)
                     Just p          = Map.lookup i par        
                     adj             = g!i                     
                     vis'            = Set.insert i vis        
                     depth'          = Map.insert i d depth    
                     i_lo            = Map.insert i d low     
--    ...     (search phase)
                     (i', adj', lo') = aux i p adj vis depth d   
                     low'            = Map.insert i lo' i_lo
                     g'              = g//[(i,adj')]
                     (expl', d', ni, par', is     ) = case i' of Just ni_1 -> (expl    ,d+1, ni_1, Map.insert ni_1 i par, True )
                                                                 Nothing   -> (expl - i,d  , p   , par                  , False)
                     ch'             = if i == 1 && is == True then ch + 1 else ch
getArtPoints False i d depth low par vis g expl ch res = getArtPoints is ni'' d'' depth  low'' par'' vis  g'' expl'' ch' res'
               where
-- Second case (init phase)
                    Just p              = Map.lookup i par 
                    adj                 = g!i
                    ii                  = head adj
                    tt                  = tail adj
                    Just d_i            = Map.lookup i depth
                    Just low_ni         = Map.lookup ii low
                    Just low_i          = Map.lookup i  low
                    res'                = if low_ni >= d_i then Set.insert i res else res
                    low_i'              = low_i `min` low_ni
--   ...       (search phase)
                    (i'', adj'', lo'')  = aux i p tt vis depth low_i'
                    low''               = Map.insert i lo'' low
                    g''                 = g//[(i, adj'')]
                    (expl'', d'', ni'', par'', is     ) = case i'' of Just ni_2 -> (expl    , d+1, ni_2, Map.insert ni_2 i par, True )
                                                                      Nothing   -> (expl - i, d  , p   , par                  , False)
                    ch'                 = if i == 1 && is == True then ch + 1 else ch


getArticulationPoints n g = let sum = mkSum n in getArtPoints True 1 1 Map.empty Map.empty (Map.fromList [(1,1)]) Set.empty g sum 0 Set.empty

------------------------------------------------------------------------------------------------------
readInt s = BSC.readInt (BSC.dropWhile isSpace s)
 
readInteger s = BSC.readInteger (BSC.dropWhile isSpace s)

readMany
  :: (BSC.ByteString -> Maybe (t, BSC.ByteString))
   -> BSC.ByteString -> ([(t, Integer)], BSC.ByteString)
readMany readf s = case readf s of
        Just (x, r) -> let (xs, t) = readMany readf t'
                       in  ((x,y) : xs, t)
                                where
                                        Just (y, t') = readInteger r
        Nothing     -> ([], s)

--------------------------------------------------------------------------------------------------
main = do
        all <- BS.getContents
        let Just (n, r1) = readInteger all
            Just (k, r2) = readInteger r1
            (x,_)        = readMany readInteger r2
            graph        = mkGraph (1,n) x
            (ch, ap')    = getArticulationPoints n graph
            ap''         = Set.delete 1 ap'
            apList       = Set.toList ap''
            ap           = if ch > 1 then 1 : apList else apList
            result       = length ap
        print result
 
  
