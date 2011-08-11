module Index where

import qualified Data.Char as Char
import qualified Data.Set as Set

data ParserState = ParserState { tokens :: [[Char]] 
                               , currentToken :: [Char]
                               , input :: [Char]
                               } deriving Show
                   
_tokenize tokens@(_) currentToken@(_) "" = 
  ParserState (registerToken currentToken tokens) "" ""

_tokenize tokens currentToken (input:inputs) = 
  if isPunctuation input 
  then _tokenize (registerToken currentToken tokens)  "" inputs
  else _tokenize tokens (input : currentToken) inputs
       
tokenize s = tokens $ _tokenize [] "" s

registerToken currentToken tokens =
  if (length currentToken == 0) then tokens else (map Char.toLower $ reverse currentToken) : tokens
                                          
isPunctuation char = Set.member char $ Set.fromList ['.', ',', ' ']
               
               


