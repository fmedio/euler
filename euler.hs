
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
divides n primes = 
    let prime = head primes in
    if mod n prime == 0 then True else divides n (tail primes)

                
primeGen [] primes = primes

primeGen numbers primes =
    let number = head numbers in
    if divides number primes
    then primeGen (tail numbers) primes
    else primeGen (tail numbers) (number : primes)

primes n = primeGen [2..n] [2]

factor :: (Integral a) => a -> [a] -> [(a, a)]
factor target [] = []

factor target primes = 
    let 
        prime = head primes 
        divided = div target prime
    in
    if mod target prime == 0 
    then (prime, divided) : factor divided (tail primes)
    else factor target (tail primes)
    
