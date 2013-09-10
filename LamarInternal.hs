module Lamar where
import Data.Word
import Data.Bits

data Tree = Node {children :: [Tree]} |
            Leaf {values :: [Word32]} |
            Nil deriving Show
            

treePath :: Word64 -> [Word8]
treePath x = map (fromIntegral . (shiftR x)) [56, 48..0] 

insert :: Word64 -> Tree -> Tree
insert x t =
  let
    p = take 6 $ treePath x
    pathOffset = fromIntegral (x .&. 0x000000000000ffff)
    index = floor $ fromIntegral pathOffset / 32
    mask = shiftR (0x80000000::Word32) $ mod pathOffset 32
  in
   Leaf []


empty_node :: Int -> Tree
empty_node x = Node [Nil | i <- [1 .. x]]

update_element :: [a] -> Integer -> (a -> a) -> [a]
update_element [] _ _ = []
update_element (x:xs) index f =
  case index of
    0 -> (f x) : update_element xs (-1) f
    _ -> x : update_element xs (index - 1) f


