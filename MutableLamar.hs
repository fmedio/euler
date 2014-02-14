module MutableLamar where
       
import Data.Array.IO
import Data.Word
import Data.Bits
import Debug.Trace
import Data.HashMap


data Tree = Node {children :: IO(IOArray Int Tree)} |
            Leaf {vs :: IO(IOArray Int Word32)} |
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
pop l = Location (x l) (tail (path l)) (MutableLamar.index l) (mask l)

current :: Location -> Int
current l = fromIntegral (head (path l))::Int

updateLeaf :: Tree -> Op -> Location -> Tree
updateLeaf t op l  =
  let vals = case t of
        Leaf vs -> vs
        _       -> (newArray (0, 2047) ((fromIntegral 0)::Word32))::IO(IOArray Int Word32)
      f a = case op of
        Set -> (a .|. mask l)
        Unset -> (a .&. (complement $ mask l))
  in
   do
     value <- readArray vals (MutableLamar.index l) 
     --writeArray vals (MutableLamar.index l) (f value) 
     Leaf vals
