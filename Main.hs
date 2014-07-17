{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord

data Node a = Leaf a Int
          | Branch (Node a) (Node a)
  deriving (Show,Read,Eq)

weight :: Node a -> Int
weight (Leaf _ i) = i
weight (Branch left right) = weight left + weight right

aCount :: Ord a => [a] -> Map a Int
aCount = foldl f Map.empty
  where f m c = Map.insertWith (+) c 1 m

freqToLeaves :: Ord a => Map a Int -> [Node a]
freqToLeaves m = fmap f (Map.toList m)
                   where f (k,v) = Leaf k v

firstPass :: Ord a => [a] -> [Node a]
firstPass = freqToLeaves . aCount

huffmanEncode :: [Node a] -> Maybe (Node a)
huffmanEncode [] = Nothing
huffmanEncode [n] = Just n
huffmanEncode (n:m:ns) = huffmanEncode (sortBy (comparing weight) ((Branch n m):ns))

huffmanTable :: Ord a => Node a -> Map a [Int]
huffmanTable (Leaf c _) = Map.singleton c []
huffmanTable (Branch left right)
  = Map.union leftMap rightMap
    where leftMap  = fmap (0:) (huffmanTable left)
          rightMap = fmap (1:) (huffmanTable right)

text :: String
text = "The technique works by creating a binary tree of nodes. These can be stored in a regular array, the size of which depends on the number of symbols, n."

main :: IO ()
main = mapM_ print (Map.toList solution)
 where solution = (maybe Map.empty huffmanTable . huffmanEncode . firstPass) text
