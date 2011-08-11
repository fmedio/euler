import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.List as List

-- Problem 1
addMultiples = foldl (\ m n -> if mod n 3 == 0 || mod n 5 == 0 then m + n else m) 0
addMultiplesUntil n = addMultiples ([1..n])

-- main = print (addMultiplesUntil 999)


-- Problem 2
addEvenTerms = foldl(\ m n -> if mod n 2 == 0 then m + n else m) 0
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

genFib max n = 
    let current = fib n
    in 
      if current > max then [] else current : genFib max (n + 1)

-- main = print(addEvenTerms(genFib 4000000 0))


-- Problem 3
divides n [] = False
divides 2 _ = False
divides n (prime:primes) = 
    if mod n prime == 0 then True else divides n primes

                
factor n 1 primes = []

factor n target primes = 
    if divides n primes then factor (n + 1) target primes else
    let 
        dividend = div target n 
        remainder = mod target n
    in case (dividend, remainder) of 
      (1, _) -> [target]
      (_, 0) -> n : factor n dividend primes
      (_, _) -> factor (n + 1) target (n : primes)

doFactor n = factor 2 n [2]

-- main = print (factor 2 600851475143 [])


-- Problem 4
isPalindrome n = ((fromIntegral n) / (read (reverse (show n))::Double)) == 1.0

largestPalindrome max = maximum [ a * b | a <- [10..max], b <- [10..max], isPalindrome (a * b)]    

-- main = print (largestPalindrome 0 (allSums [1..999]))


-- Problem 5
smallestDivisor :: (Integral t) => [t] -> t 
smallestDivisor xs = foldl1 lcm xs

-- main = print (smallestDivisor [1..20])


-- Problem 6
sumOfSquares n = foldl (\ a b -> a + b * b) 0 [1..n]
squareOfSum n = (foldl (+) 0 [1..n]) ^ 2

-- main = print ((squareOfSum 100) - (sumOfSquares 100))


-- Problem 7

nthPrime max (x:xs) primes = 
    if length primes == max then head primes else
    let 
        newPrimes = if divides x primes then primes else x : primes
    in 
    nthPrime max xs newPrimes

-- 104743
-- main = print (nthPrime 10001 [2..] [])


-- Problem 8

greatestProduct :: Int -> [Char] -> Int

greatestProduct currentMax chars = 
    if length chars < 5 then currentMax else
    let 
        currentSum = foldl (\ a b -> a * digitToInt(b)::Int) 1 (take 5 chars)
        nextAttempt = if currentSum > currentMax then currentSum else currentMax
    in
        greatestProduct nextAttempt (tail chars)

-- main = print (greatestProduct 0 "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")


-- Problem 9
-- main = print ([(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 1000])        

-- Problem 12

problem12 n = head $ filter ((> n) . divisorCount) (triangles [1..])
       where 
       triangles (x:xs) = (foldl (+) 0 [1..x]) : (triangles xs)
       divisorCount n = product $ map ((+ 1) .length) (List.group (doFactor n))

-- main = print $ problem12 500


-- Problem 13

problem13 = (print . take 10 . show . sum) [10000000000, 12345678912234]


-- Problem 14

problem14 xs = foldl compareTuples (0,0) [ (x, (length . expand) x) | x <- xs ] 
               where
                 expand 1 = [1]
                 expand n = n : expand (if even n then div n 2 else 3 * n + 1)
                 compareTuples a b = if snd a > snd b then a else b

-- main = print $ problem14 [1..1000000]


-- Problem 16

problem16 = (sum . map digitToInt . show) $ 2 ^ 1000

-- main = print problem16


-- Problem 20

problem20 = sum . map digitToInt . show $ product [1..100]


-- Problem 21
divisors n = Set.fromList $ map product $ List.subsequences $ doFactor n    
sumOfDivisors n = Set.fold (+) (-n) $ divisors n
amicablePairs n = [(a,b) | a <- [1..n], b <- [a..n], sumOfDivisors a == b && sumOfDivisors b == a && a /= b]

problem21 n = foldl (\ total tuple -> total + fst tuple + snd tuple) 0 $ amicablePairs n

-- main = print $ problem21 10000


-- Problem 25

betterFib = map fst $ iterate (\(a,b) -> (b, a+b)) (0,1)
problem25 = length $ takeWhile (\a -> length (show a) < 1000) betterFib


-- Problem 29
problem29 = Set.size $ Set.fromList [ a^b | a <- [2..100] , b <- [2..100]]


-- Problem 48 
problem48 = (reverse . take 10 . reverse . show) $ foldl (\ a b -> a + (b ^ b)) 0 [1..1000]


-- Problem 52
problem52 = ((+) 1) . length $ takeWhile (\a -> not $ compareMultiple a [2..6]) [1..]
            where
              compareDigits a b = (List.sort $ show a) == (List.sort $ show b)
              compareMultiple n [] = True
              compareMultiple n (x:xs) = if not $ compareDigits n (x * n) then False else compareMultiple n xs


