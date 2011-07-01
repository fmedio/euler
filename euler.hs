
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
divides n primes = 
    let prime = head primes in
    if mod n prime == 0 then True else divides n (tail primes)

                
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

main = print (factor 2 600851475143 [])
