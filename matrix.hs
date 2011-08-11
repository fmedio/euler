-- Toying with non-negative matrix factorization. 
-- See http://hebb.mit.edu/people/seung/papers/nmfconverge.pdf for details.

module Matrix where

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

(***) :: (Num a) => [[a]] -> [[a]] -> [[a]]
[[]] *** _ = [[]]
_ *** [[]] = [[]]
a *** b = [map (sum . zipWith (*) r) $ transpose b | r <- a]

apply ::  (a -> a-> a) -> [[a]] -> [[a]] -> [[a]]
apply f x y = zipWith (\ a b -> zipWith f a b) x y

(///) :: (Fractional a) => [[a]] -> [[a]] -> [[a]]
x /// y = apply (/) x y

(^^^) :: (Fractional a) => [[a]] -> [[a]] -> [[a]]
x ^^^ y = apply (*) x y


update :: (Fractional a) => [[a]] -> ([[a]], [[a]]) -> ([[a]], [[a]])
update v (w, h) = (w', h')
                   where                  
                   h' = h ^^^ (transpose w *** v) /// (transpose w *** w *** h)
                   w' = w ^^^ (v *** transpose h') /// (w *** h' *** transpose h')

divergence [] [] = 0
divergence (x:xs) (y:ys) = (sum $ zipWith (\ a b -> (a - b)^2) x y) + divergence xs ys

emptyMatrix :: (Integral a, Fractional b) => a -> a -> b -> [[b]]
emptyMatrix x y n = map (\ b -> map (\ a -> n ) [1..y]) [1..x]

factor :: (Fractional a, Integral b) => [[a]] -> ([[a]], [[a]]) -> b -> ([[a]], [[a]])
factor v (w, h) 0 = (w, h)
factor v (w, h) n = factor v (update v (w, h)) (n - 1)

matrixWidth x = length x
matrixLength x = length $ head x

doFactor v dimensions iterations = factor v (w, h) iterations
                        where 
                          w = emptyMatrix (matrixWidth v) dimensions 1
                          h = emptyMatrix dimensions (matrixLength v) 1

