module LamarInternal (Tree(Leaf,Node,Nil),
                      Op(Set,Unset),
                      treePath,
                      values,
                      updateElement,
                      updateLeaf,
                      toWord32) where
import Data.Word
import Data.Bits

data Tree = Node {children :: [Tree]} |
            Leaf {values :: [Word32]} |
            Nil deriving (Show, Eq)
            
data Op = Set | Unset

treePath :: Word64 -> [Word8]
treePath x = take 6 $ map (fromIntegral . (shiftR x)) [56, 48..0] 

updateLeaf :: Tree -> Op -> Word64 -> Tree
updateLeaf t op x =
  let values = case t of
        Nil -> map toWord32 [0 | i <- [1 .. 2048]]
        Leaf xs -> xs
        Node children -> map toWord32 [0 | i <- [1 .. 2048]]
      pathOffset = fromIntegral (x .&. 0x000000000000ffff)
      index = floor $ fromIntegral pathOffset / 32
      mask = shiftR (0x80000000::Word32) $ mod pathOffset 32
      f a = case op of
        Set -> (a .|. mask)
        Unset -> (a .&. (complement mask))
      updated = updateElement values index f
  in
   Leaf updated
   
insert :: Word64 -> Tree -> Tree
insert x t =
  let
    p = take 6 $ treePath x
    pathOffset = fromIntegral (x .&. 0x000000000000ffff)
    index = floor $ fromIntegral pathOffset / 32
    mask = shiftR (0x80000000::Word32) $ mod pathOffset 32
  in
   Leaf []

toWord32 :: Int -> Word32                             
toWord32 x = (fromIntegral x) :: Word32

updateElement :: [a] -> Integer -> (a -> a) -> [a]
updateElement [] _ _ = []
updateElement (x:xs) index f =
  case index of
    0 -> (f x) : updateElement xs (-1) f
    _ -> x : updateElement xs (index - 1) f


