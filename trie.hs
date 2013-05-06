data Trie a =  
     Trie { value    :: Maybe a
          , children :: [Trie a] } deriving Show


sequenceContains :: (Eq a) => a -> [a] -> Bool
sequenceContains a [] = False
sequenceContains a (x:xs) = if (a == x) then True else sequenceContains a xs

          
insert :: (Eq a) => [a] -> Trie a -> Trie a
insert [] t = t
insert (x:xs) t = 
       if sequenceContains (Just x) (map (\e -> value e) (children t)) then 
       Trie (value t) $ map (\e -> if Just x == value e 
                                   then insert xs e
                                   else e) 
                             $ children t
       else 
       Trie (value t) ((insert xs (Trie (Just x) [])) : (children t))

