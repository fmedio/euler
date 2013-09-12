module LamarInternal (Tree(Leaf,Node,Nil),
                      values,
                      Op(Set,Unset),
                      Location(x, path, index, mask),
                      location,
                      updateSequence,
                      updateLeaf,
                      get,
                      update) where
import Data.Word
import Data.Bits
--import Debug.Trace

data Tree = Node {children :: [Tree]} |
            Leaf {values :: [Word32]} |
            Nil deriving (Show, Eq)

data Op = Set | Unset

data Location = Location {
  x :: Word64,
  path :: [Word8],
  index :: Int,
  mask :: Word32 } deriving (Show, Eq)

location :: Word64 -> Location 
location x =
  let
    path = take 6 $ map (fromIntegral . (shiftR x)) [56, 48..0] 
    pathOffset = fromIntegral (x .&. 0x000000000000ffff)
    index = floor $ fromIntegral pathOffset / 32
    mask = shiftR (0x80000000::Word32) $ (mod pathOffset 32)
  in Location x path index mask

pop :: Location -> Location
pop l = Location (x l) (tail (path l)) (index l) (mask l)

current :: Location -> Integer
current l = toInteger (head (path l))

updateLeaf :: Tree -> Op -> Location -> Tree
updateLeaf t op l  =
  let values = case t of
        Leaf xs -> xs
        _ -> [(fromIntegral 0)::Word32 | i <- [1 .. 2048]]
      f a = case op of
        Set -> (a .|. mask l)
        Unset -> (a .&. (complement $ mask l))
      updated = updateSequence values (toInteger (index l)) f
  in
   Leaf updated

updateTree :: Location -> Op -> Tree -> Tree
updateTree l op t =
  case (path l) of
    []     -> updateLeaf t op l
    (p:ps) -> case t of
      Node children   -> Node (updateSequence children (current l) (updateTree (pop l) op))
      _               -> Node (updateSequence [Nil | i <- [1 .. 256]] (current l) (updateTree (pop l) op))


update :: Tree -> Op -> Word64 -> Tree
update t op x = case t of
  Node children -> updateTree (location x) op t
  _             -> updateTree (location x) op (Node [Nil | i <- [1 .. 256]])

get :: Tree -> Word64 -> Bool
get t x = get' t (location x)
  
get' :: Tree -> Location -> Bool
get' t l = case t of
  Leaf values     -> ((values !! (index l)) .&. (mask l)) /= 0
  Node children   ->
    let
      i = current l
      child = (children !! (fromIntegral(i)::Int))     
    in
      get' child (pop l)    
  Nil             -> False


updateSequence :: [a] -> Integer -> (a -> a) -> [a]
updateSequence [] _ _ = []
updateSequence (x:xs) index f =
  case index of
    0 -> (f x) : updateSequence xs (-1) f
    _ -> x : updateSequence xs (index - 1) f


