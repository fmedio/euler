data Trie a =  
     Trie { value    :: Maybe a
          , children :: [Trie a] } deriving Show


find :: (Eq a) => a -> [Trie a] -> Maybe (Trie a)
find a [] = Nothing
find a (t:ts) = if (Just a) == value t then (Just t) else find a ts

          
insert :: (Eq a) => [a] -> Trie a -> Trie a
insert [] t = t
insert (x:xs) t = 
       case find x (children t) of
       Nothing   -> Trie (value t) ((insert xs (Trie (Just x) [])) : (children t))
       _         -> Trie (value t) $ map (\e -> if Just x == value e 
                                         then insert xs e
                                         else e) 
                                   $ children t

contains :: (Eq a) => [a] -> Trie a -> Bool
contains [] t = True
contains (x:xs) t = let child = find x (children t) in
         maybe False (\e -> contains xs e) child