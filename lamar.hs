import Data.Word
import Data.Bits

data Tree = Node {children :: [Tree]} |
            Leaf {values :: [Word32]} |
            Nil deriving Show
            

path :: Word64 -> [Word8]
path x = map (fromIntegral . (shiftR x)) [56, 48..0] 

insert :: Word64 -> Tree -> Tree
insert x t =
  let
    p = take 6 $ path x
    pathOffset = fromIntegral (x .&. 0x000000000000ffff)
    index = floor $ fromIntegral pathOffset / 32
    mask = shiftR (0x80000000::Word32) $ mod pathOffset 32
  in
   _update p index (.|. mask) t

_update :: [Word8] -> Integer -> (Word32 -> Word32) -> Tree -> Tree
_update [] index apply_mask t =
  case t of
    Nil           -> let empty = [0 | i <- [1 .. 2048]]
                         values = update_element empty index apply_mask
                     in Leaf values
    Leaf values   -> Leaf (update_element values index apply_mask)
    _             -> Leaf [] 
_update (p:ps) index apply_mask t =
  case t of
    Node children -> let path_element = p
                         children = update_element children (toInteger p) (_update ps index apply_mask)
                     in Node children
    Nil           -> let empty = [Nil | i <- [1 .. 256]]
                         children = update_element empty (toInteger p) (_update ps index apply_mask)
                     in Node children
    _             -> Leaf []



empty_node :: Int -> Tree
empty_node x = Node [Nil | i <- [1 .. x]]

update_element :: [a] -> Integer -> (a -> a) -> [a]
update_element [] _ _ = []
update_element (x:xs) index f =
  case index of
    0 -> (f x) : update_element xs (-1) f
    _ -> x : update_element xs (index - 1) f
