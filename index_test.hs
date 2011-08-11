module Index.Test where

import Index
import Test.HUnit

tests = test [
  "testTokenize" ~: "" ~: ["bar", "foo"] ~=? (tokenize "foo bar"),
  "testTokenizePunctuation" ~: "" ~: ["panda", "bar", "foo"] ~=? (tokenize "foo, bar. panda"),
  "testCase" ~: "" ~: ["panda", "bar", "foo"] ~=? (tokenize "fOo, bar. pandA")
                  ]
