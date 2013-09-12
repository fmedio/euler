module Main where

import LamarInternal
import Debug.Trace

main :: IO ()
main = let tree = {-# SCC "build_tree" #-} foldr (\ x t -> update t Set x) Nil [1..10000]
           c =  {-# SCC "cardinality" #-} cardinality tree
       in
       do putStrLn ("Cardinality: " ++ (show c))
