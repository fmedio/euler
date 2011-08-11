module Index.Test where

import Index
import Test.HUnit

tests = test [
  "testTokenize" ~: "" ~: ["bar", "foo"] ~=? (tokenize "foo bar"),
  "testTokenizePunctuation" ~: "" ~: ["panda", "bar", "foo"] ~=? (tokenize "foo, bar. panda"),
  "testMixedCase" ~: "" ~: ["panda", "bar", "foo"] ~=? (tokenize "fOo, bar. pandA"),
  "testTermDocMatrix" ~: "" ~: [[3,2],[1,4]] ~=? (termDocMatrix ["x y x y x", "y x y y y"])
  
             ]
