module Index where

import qualified Data.Char as Char
import qualified Data.Set as Set

data ParserState = ParserState { tokens :: [[Char]] 
                               , currentToken :: [Char]
                               , input :: [Char]
                               } deriving Show
                                          
_tokenize tokens@(_) currentToken@(_) input@("") =
  if length currentToken /= 0 then
    ParserState (doctorToken currentToken : tokens) "" ""
  else
    ParserState tokens "" "" 
    
_tokenize tokens currentToken (input:inputs) =
  let (moreTokens, moreCurrentToken) = filter tokens currentToken in
  _tokenize moreTokens moreCurrentToken inputs
  where 
    filter tokens currentToken = 
      if isPunctuation input then
        if length currentToken == 0 then
          (tokens, "")
        else
          (doctorToken currentToken : tokens, "")
      else
        (tokens, input : currentToken)
          
tokenize s = tokens $ _tokenize [] "" s   

doctorToken token = map Char.toLower $ reverse token

isPunctuation char = Set.member char $ Set.fromList ['.', ',', ' ']
               



