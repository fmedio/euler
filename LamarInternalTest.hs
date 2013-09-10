module Test where

import Data.Word
import Test.HUnit
import LamarInternal

test1 = TestCase (assertEqual "" (1,2) (1,2))
test2 = TestCase (assertEqual "" (1,2) (1,2))

testTreePath = TestCase (assertEqual "treePath" [0,0,0,0,1,101] (treePath 23423423))

testUpdateBlankLeaf = TestCase
                 (let empty = map toWord32 [0 | i <- [1 .. 2048]]
                      expected = updateElement empty 1 (\ a -> 1)
                  in
                   assertEqual "testUpdateBlankLeaf" (Leaf expected) (updateLeaf Nil Set 63))

testUpdateExistingLeaf = TestCase
                 (let one = updateLeaf Nil Set 63
                      two = updateLeaf one Unset 63
                  in
                   assertEqual "testUpdateExistingLeaf" 0 (values two !! 1))

                         
tests = TestList [testTreePath, testUpdateBlankLeaf, testUpdateExistingLeaf]

