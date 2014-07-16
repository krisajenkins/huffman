{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord

data Node = Leaf { symbol :: Char, weight :: Integer }
          | Internal { left :: Node, right :: Node, weight :: Integer }
  deriving (Show,Read,Eq)

frequency :: String -> Map Char Integer
frequency = foldr f Map.empty
  where f :: Char -> Map Char Integer -> Map Char Integer
        f c = Map.insertWith (+) c 1

freqToLeaves :: Map Char Integer -> [Node]
freqToLeaves m = fmap f (Map.toList m)
                   where f (k,v) = Leaf { symbol = k, weight = v}

firstPass :: String -> [Node]
firstPass = freqToLeaves . frequency

huffmanEncode :: [Node] -> Maybe Node
huffmanEncode [] = Nothing
huffmanEncode [n] = Just n
huffmanEncode (n:m:ns) = huffmanEncode $ sortBy (comparing weight) (x:ns)
                         where x = Internal {left = n, right = m, weight = total}
                               total = (+) (weight n) (weight m)

huffmanTable :: Num a => Maybe Node -> Map Char [a]
huffmanTable Nothing = Map.empty
huffmanTable (Just (Leaf {symbol = s})) = Map.singleton s []
huffmanTable (Just (Internal {left = l, right = r})) = Map.union leftMap rightMap
                                                        where leftMap  = Map.map (0:) (huffmanTable (Just l))
                                                              rightMap = Map.map (1:) (huffmanTable (Just r))

text :: String
text = "The technique works by creating a binary tree of nodes. These can be stored in a regular array, the size of which depends on the number of symbols, n."

main :: IO ()
main = mapM_ (print . show) (Map.toList solution)
 where solution = huffmanTable $ huffmanEncode $ firstPass text
