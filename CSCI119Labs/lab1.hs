---- CSci 119, Lab 1 ----

-- Note: you will replace all instances of "undefined" below with your answers.


---- Boolean operators

-- The following code tests whether "and" is commutative:
bools = [True, False]
and_commutes = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Write similar defintions that test whether "or" is commutative,
-- "and" and "or" are associative, "and" distributes over "or",
-- "or" distributes over "and"
or_commutes = and  [(p || q) == (q || p) | p <- bools, q <- bools]
and_assoc = and [(p && (q && r)) == (r && (p && q)) | p <- bools, q <- bools, r <-bools]
or_assoc = and [(p || (q || r)) == (r || (p || q)) | p <- bools, q <- bools, r <-bools]
and_dist = and [(p && (q || r)) == ((p && q) || (p && r)) | p <- bools, q <- bools, r <-bools]
or_dist = and [(p || (q && r)) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <-bools]
          
-- The exclusive-or operation on Bool in Haskell is equivalent to /=.
-- Test the properties of this operation (commutativity, associativity,
-- distributivity over and+or, and distributivity of and+or over it)
-- using the same method as above
xor_commutes = and [(p /= q) == (q /= p) | p <- bools, q <- bools]
xor_assoc    = and [(p /= (q /= r)) == (r /= (p /= q)) | p <- bools, q <- bools, r <-bools]
xor_dist_and = and [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <-bools]
xor_dist_or  = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r <-bools]
and_dist_xor = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <-bools]
or_dist_xor  = and [(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <-bools]
               
-- The implication operator on Bool in Haskell is equivalent to <=.
-- Check whether implication is associative or commutative:
assoc_imp = and [(p <= (q <= r)) == (r <= (p <= q)) | p <- bools, q <- bools, r <- bools]
comm_imp = and [(p <= q) == (q <= p) | p <- bools, q <- bools]


----- Evaluating statements involving quantifiers

-- Assume that the universe of discourse is the set {1,2,3,4,5,6,7,8},
-- that is, that the word "number" temporarily means 1, 2, ..., 8.
-- Your solutions to the problems below should work no matter what
-- finite list of integers u is. For example, u = [5, 2, 17, 58, 21].

u = [1..8]

-- Translate each of the following statements first, in a comment, into a
-- logical statement involving forall, exists, and, or, imp, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. I'll work one example.

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) imp (n > 1)
prob1_1 = and [(n > 2) <= (n > 1) | n <- u]   -- direct translation
prob1_2 = and [n > 1 | n <- u, n > 2]         -- using bounded quantifier

-- 2. Every number is either greater than 1 or less than 2
-- A: forall n, (n > 1) or (n < 2)
prob2 = and [(n > 1) || (n < 2) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall x forall y, (x <= y) or (y <= x)
prob3 = and [(x <= y) || (y <= x) | x <- u, y <- u]

-- 4. There is an odd number greater than 4
-- A: ThereExists n, odd n and (n > 4)
prob4 = or [(n > 4) | n <- u, odd n]

-- 5: There are two odd numbers that add up to 10
-- A: thereExists n y, such that n and y = odd and add to 10
prob5 = or [(n + y == 10) || (y + n == 10) | n <- u, y <- u, odd n, odd y]

-- 6. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forAll odd n, thereExists even y such that (y > n)
prob6 = and [or [(y > n) | y <- u, even y] | n <- u, odd n]

-- 7. For every even number, there is a greater odd number
-- A: forAll even n, thereExists odd y such that (y > n)
prob7 = and [or [(y > n) | y <- u, odd y] | n <- u, even n]

-- 8. There are two odd numbers that add up to 6
-- A: thereExists odd n, thereExists odd y such that n + y = 6
prob8 = or [ (n + y == 6) | n <- u, y <- u, odd n, odd y]

-- 9. There is a number that is at least as large as every number
--    (i.e., according to >=)
-- A: thereExists n, forAll y such that (n >= x)
prob9 = or[and[ (n >= y) | y <- u] | n <- u]

-- 10. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: forAll n, thereExists y such that n /= y
--thought process, prove that they are # neighbors i.e. 1,2 or 2,3 not 1,3..
prob10 = and[ or [ not(y < x && x < n) | y <- u, x <- u, y /= n ] | n <- u]

{-
Testing:
*Main> or_commutes
True
*Main> and_assoc
True
*Main> or_assoc
True
*Main> and_dist
True
*Main> or_dist
True
*Main> xor_commutes
True
*Main> xor_assoc
True
*Main> xor_dist_and
False
*Main> xor_dist_or
False
*Main> and_dist_xor
True
*Main> or_dist_xor
False
*Main> assoc_imp
False
*Main> comm_imp
False
*Main> prob2
True
*Main> prob3
True
*Main> prob4
True
*Main> prob5
True
*Main> prob6
True
*Main> prob7
False
*Main> prob10
True
-}