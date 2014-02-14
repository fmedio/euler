import Data.Array.IO
import Data.IORef

data Poop = Poop { value :: Int } deriving Show

foo x =
  do
    arr <- newArray(0,10) x :: IO (IOArray Int Int)
    v <- getElems arr
    return v





