
module Misc (the,nub,hist,collate,zipMap,loadBytes) where

import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS (readFile,unpack)

the :: String -> [a] -> a
the s = \case [x] -> x; xs -> error (show ("the",s,length xs))

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

hist :: Ord a => [a] -> Map a Int
hist ks = Map.fromList [ (k,length xs) | (k,xs) <- collate [ (k,()) | k <- ks ] ]

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

zipMap :: Ord k => Map k v1 -> Map k v2 -> Map k (v1,v2)
zipMap = Map.intersectionWith (\a b -> (a,b))

loadBytes :: FilePath -> IO [Word8]
loadBytes path = BS.unpack <$> BS.readFile path
