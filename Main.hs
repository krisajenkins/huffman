{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord

data Node = Leaf { symbol :: Char, frequency :: Integer }
          | Branch { left :: Node, right :: Node }
  deriving (Show,Read,Eq)

weight :: Node -> Integer
weight (Leaf {frequency = f}) = f
weight (Branch {left = l, right = r}) = weight l + weight r

charCount :: String -> Map Char Integer
charCount = foldr f Map.empty
  where f c = Map.insertWith (+) c 1

freqToLeaves :: Map Char Integer -> [Node]
freqToLeaves m = fmap f (Map.toList m)
                   where f (k,v) = Leaf { symbol = k, frequency = v}

firstPass :: String -> [Node]
firstPass = freqToLeaves . charCount

huffmanEncode :: [Node] -> Maybe Node
huffmanEncode [] = Nothing
huffmanEncode [n] = Just n
huffmanEncode (n:m:ns) = huffmanEncode (sortBy (comparing weight) (x:ns))
  where x = Branch {left = n, right = m}

huffmanTable :: Maybe Node -> Map Char [Integer]
huffmanTable Nothing = Map.empty
huffmanTable (Just (Leaf {symbol = s})) = Map.singleton s []
huffmanTable (Just (Branch {left = l, right = r}))
  = Map.union leftMap rightMap
    where leftMap  = fmap (0:) (huffmanTable (Just l))
          rightMap = fmap (1:) (huffmanTable (Just r))

text :: String
text = "The technique works by creating a binary tree of nodes. These can be stored in a regular array, the size of which depends on the number of symbols, n."

main :: IO ()
main = mapM_ print (Map.toList solution)
 where solution = (huffmanTable . huffmanEncode . firstPass) text
