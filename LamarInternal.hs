module LamarInternal (Tree(Leaf,Node,Nil),
                      values,
                      Op(Set,Unset),
                      Location(x, path, index, mask),
                      location,
                      updateLeaf,
                      get,
                      LamarInternal.update,
                      cardinality,
                      bitsSet) where
import Data.Word
import Data.Bits
import Debug.Trace
import Data.HashMap


data Tree = Node {children :: Map Int Tree} |
            Leaf {values :: Map Int Word32} |
            Nil 

data Op = Set | Unset

data Location = Location {
  x :: Word64,
  path :: [Word8],
  index :: Int,
  mask :: Word32 } deriving (Show, Eq)

location :: Word64 -> Location 
location x =
  let
    path = take 6 $ Prelude.map (fromIntegral . (shiftR x)) [56, 48..0] 
    pathOffset = fromIntegral (x .&. 0x000000000000ffff)
    index = floor $ fromIntegral pathOffset / 32
    mask = shiftR (0x80000000::Word32) $ (mod pathOffset 32)
  in Location x path index mask
  
pop :: Location -> Location
pop l = Location (x l) (tail (path l)) (LamarInternal.index l) (mask l)

current :: Location -> Int
current l = fromIntegral (head (path l))::Int

updateLeaf :: Tree -> Op -> Location -> Tree
updateLeaf t op l  =
  let values = case t of
        Leaf values -> values
        _           -> empty
      f a = case op of
        Set -> (a .|. mask l)
        Unset -> (a .&. (complement $ mask l))
  in
   let
     value = findWithDefault 0 (LamarInternal.index l) values
     updatedValues = insert (LamarInternal.index l) (f value) values
   in
    Leaf updatedValues

updateTree :: Location -> Op -> Tree -> Tree
updateTree l op t =
  case (path l) of
    []     -> updateLeaf t op l
    (p:ps) -> 
      let nodes = case t of {Node children -> children; _ -> empty}
      in
       let child = findWithDefault Nil (current l) nodes
           newChildren = insert (current l) (updateTree (pop l) op child) nodes
       in Node newChildren


update :: Tree -> Op -> Word64 -> Tree
update t op x = case t of
  Node children -> updateTree (location x) op t
  _             -> updateTree (location x) op (Node empty)

get :: Tree -> Word64 -> Bool
get t x = get' t (location x)
  
get' :: Tree -> Location -> Bool
get' t l = case t of
  Leaf values     -> ((findWithDefault 0 (LamarInternal.index l) values) .&. (mask l)) /= 0
  Node children   ->
    let
      i = current l
      child = findWithDefault Nil i children 
    in
      get' child (pop l)    
  Nil             -> False

cardinality :: Tree -> Word64
cardinality t =
  case t of
    Nil           -> 0
    Leaf values   -> fold (\v count -> count + (fromIntegral $ bitsSet v)::Word64) 0 values
    Node children -> fold (\t count -> count + (cardinality t)) 0 children
                                      
bitsSet :: Word32 -> Int
bitsSet 0 = 0
bitsSet x =
  let last = (fromIntegral (x .&. 0x00000001))::Int in
  case x of
    0 -> 0
    _ -> last + bitsSet (shiftR x 1)

