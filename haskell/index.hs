module Index(tokenize, termDocMatrix) where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Matrix                                          

parse tokens@(_) currentToken@(_) "" =
  if length currentToken /= 0 then
    doctorToken currentToken : tokens
  else
    tokens
    
parse tokens currentToken (input:inputs) =
  let (moreTokens, moreCurrentToken) = filter tokens currentToken in
  parse moreTokens moreCurrentToken inputs
  where 
    filter tokens currentToken = 
      if isPunctuation input then
        if length currentToken == 0 then
          (tokens, "")
        else
          (doctorToken currentToken : tokens, "")
      else
        (tokens, input : currentToken)
          
tokenize s = parse [] "" s   

doctorToken token = reverse . map Char.toLower $ token

isPunctuation char = Set.member char $ Set.fromList ['.', ',', ' ']
               
indexOneDoc terms termVector [] = (terms, termVector)

indexOneDoc terms termVector (token:tokens) = 
  let count = Map.findWithDefault 0 token termVector in
  indexOneDoc (Set.insert token terms) (Map.insert token (count + 1) termVector) tokens
  
indexAll terms termVectors [] = (terms, termVectors)

indexAll terms termVectors (doc:docs) = 
  let (terms', termVector) = indexOneDoc terms Map.empty $ tokenize doc in
  indexAll terms' (termVector : termVectors) docs
  
termDocMatrix docs = 
  let 
    (termSet, termVectors) = indexAll Set.empty [] docs 
    terms = Set.toList termSet
    matrix = map (\ termVector -> map (\ term -> (Map.findWithDefault 0.0 term termVector)::Double ) terms) termVectors
  in
   (terms, matrix)
  


  
  
