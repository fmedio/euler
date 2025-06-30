module Test where

import Data.Word
import Test.HUnit
import LamarInternal

test1 = TestCase (assertEqual "" (1,2) (1,2))
test2 = TestCase (assertEqual "" (1,2) (1,2))

pipeline :: a -> [a -> a] -> a
pipeline x [] = x
pipeline x (f:fs) = pipeline (f x) fs


tests = TestList [
  TestCase (let loc = location 0 in
             assertEqual "mask 0" (2^31) (mask loc)),

  TestCase (let loc = location 1 in
             assertEqual "mask 1" (2^30) (mask loc)),

  TestCase (let loc = location 32 in
             assertEqual "mask 32" (2^31) (mask loc)),
  
  TestCase (let loc = location 32 in
             assertEqual "index 32" 1 (index loc)),

  TestCase (let tree = update Nil Set 1 in
             assertBool "set 1" (get tree 1)),
  
  TestCase (let tree = update Nil Set 32 in
             assertBool "set 32" (get tree 32)),

  TestCase (let tree = pipeline Nil
                       [
                         (\t -> update t Set 32),
                         (\t -> update t Unset 32)
                       ]
            in assertBool "set unset 32" $ not (get tree 32)),

  TestCase (let tree = foldr (\ x t -> update t Set x) Nil [1..10]
            in assertEqual "cardinality 10" 10 (cardinality tree)),

  TestCase (assertEqual "bits set in 5" 2 (bitsSet 5))
  ]
             

