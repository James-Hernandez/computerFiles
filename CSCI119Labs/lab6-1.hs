-- Lab 6:  FSM constructions for regular operators

import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)


---------------- Your solution to Lab 5, ported to FSM a -------------------
checkFSM ::Eq a => FSM a -> Bool
checkFSM (qs, s, fs, d) = nodups qs && s `elem` qs && all (`elem` qs) fs && func
  where nodups [] = True
        nodups (q:qs) = q `notElem` qs && nodups qs
        func = and [d q a `elem` qs | q <- qs, a <- sigma]


-- Gives the delta* function (recursive in w)
dstar ::Eq a => FSM a -> a -> [Char] -> a
dstar (_, _, _, d) = foldl d


-- Machine acceptance, Definition 1 (via delta*)
accept1 :: Eq a => FSM a-> [Char] -> Bool
accept1 m@(qs, s, fs, d) w = dstar m s w `elem` fs


-- Machine acceptance, Definition 2 (via L_q(M))
-- accept2 ::Eq a => FSM a -> [Char] -> Bool
-- accept2 (qs, s, fs, d) w = aux s w where
--   -- aux q w = whether m, starting in q, accepts w (recursive in w)
--   aux :: Eq a => a -> [Char] -> Bool
--   aux q [] = q `elem` fs
--   aux q (x:xs) = aux (d q x) xs

-- Define a machine that accepts exactly the strings with an odd number of 'b's
-- (Solution given for illustration)
oddbs :: FSM Int
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

evenbs :: FSM Int
evenbs = ([0,1], 0, [0],d) where
  d q 'a' = q 
  d q 'b' = 1 - q

end_ba :: FSM Int
end_ba = ([0,1,2],0,[2],d) where
  d q 'b' = 1
  d q 'a' = if q == 1 then 2 else 0
 

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM Int
avoid_aab = ([0,1,2,3], 0, [0,1,2], d) where
  d q 'a' = if q <= 1 then q+1 else q
  d q 'b' = if q <= 1 then 0 else 3
  ---- or equivalently and less cleverly
  -- d 0 'a' = 1;   d 0 'b' = 0
  -- d 1 'a' = 2;   d 1 'b' = 0
  -- d 2 'a' = 2;   d 2 'b' = 3
  -- d 3 _ = 3


  -- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM Int
end_ab = ([0,1,2], 0, [2], d) where
  d q 'a' = 1
  d q 'b' = if q == 1 then 2 else 0


-- Define a function that takes a string and returns a machine that accepts
-- exactly that string and nothing else (compare to 'onestr' from Lab 3)
-- Hint: the expression w !! i gives the i-th character of the list/string w
exactly :: String -> FSM Int
exactly w = ([0..n+1], 0, [n], d) where
  n = length w
  d i x = if i < n && w !! i == x then i+1 else n+1

---------------- Some additional useful functions --------------------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product (preserves normalization)
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]

-- Powerset  (preserves normalization)
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Check whether two lists overlap
overlap :: Eq a => [a] -> [a] -> Bool
overlap [] ys = False
overlap (x:xs) ys = elem x ys || overlap xs ys


---------------- Lab 6 begins here -----------------------------------------

-- Complete the FSM constructions for the regular expression operators
-- and test your functions adquately


-- Machine that accepts the empty language
emptyFSM :: FSM Int
emptyFSM = ([0],0,[0],d)
  where d _ _ = 0


-- Machine that accepts the language {"a"} where a in sigma
letterFSM :: Char -> FSM Int
letterFSM a = (qs,s,fs,d) where
  qs = [0,1,2]
  s = 0
  fs = [1]
  d 0 a1
    | a1 == a = 1
    | otherwise = 2


-- Machine that accepts the union of the languages accepted by m1 and m2
unionFSM :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a, b)
unionFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1,s2)
  fs = [(q1,q2) |q1 <- qs1, q2 <- qs2, elem q1 fs1 || elem q2 fs2 ]
  d (q1,q2) a = (d1 q1 a, d2 q2 a)

-- Machine that accepts the concatenation of the languages accepted by m1 and m2
catFSM :: (Eq a, Ord b) => FSM a -> FSM b -> FSM (a, [b])
catFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< power qs2
  s  = (s1,[s2 | elem s1 fs1])
  fs = [(q1,x) | q1 <- qs1, x <- power qs2 ,overlap x fs2]
  d (q1,x) a = (q',correct q' (norm [d2 q2 a | q2 <- x])) where
    q'= d1 q1 a
  correct q' d = norm (if elem q' fs1 then s2:d else d)



-- Machine that accepts the Kleene star of the language accepted by m1
starFSM :: Ord a => FSM a -> FSM [a]
starFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = power qs1
  s  = []
  fs = []:[q1 | q1 <- qs, null q1 || overlap q1 fs1]
  d x a
    | x == [] = [d1 s1 a]
    | otherwise = undefined
  -- d x a = if x == [] then correct (d1 s1 a) else correct [d q a | q <- qs] where
  --   --c = if overlap x fs then x:s1 else x
  --   correct d = if overlap x fs then x:s1 else x
  --   -- | x:s1 if overlap x fs
  --   -- | otherwise = x
  --   -- else correct c (d q a) where



---------------- Bonus Features (for testing and experimenting) ------------

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d) where
  qs' = uclosure [s] (\q -> map (d q) sigma)
  fs' = filter (`elem` fs) qs'

-- Change the states of an FSM from an equality type to Int 
-- and use an array lookup for the transition function
intify :: Eq a => FSM a -> FSM Int
intify (qs, s, fs, d) = ([0..n-1], s', fs', d') where
  n = length qs
  m = length sigma
  s'  = ind qs s
  fs' = map (ind qs) fs
  arr = listArray ((0,0), (n-1,m-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':qs) q = if q == q' then 0 else 1 + ind qs q

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable

---- Regular expressions, along with output and input
data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp

instance (Show RegExp) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Let c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Let x:rs)

-- Use constructions above to get reduced machine associated with regex
-- Warning: it can take a lot of time/memory to compute these for "big" regex's
-- We will see much better ways later in the course
re2fsm :: RegExp -> FSM Int
re2fsm Empty = emptyFSM
re2fsm (Let c) = letterFSM c
re2fsm (Union r1 r2) = reduce $ unionFSM (re2fsm r1) (re2fsm r2)
re2fsm (Cat r1 r2) = reduce $ catFSM (re2fsm r1) (re2fsm r2)
re2fsm (Star r1) = reduce $ starFSM (re2fsm r1)

{-
*Main> (qs,s,fs,d) = unionFSM oddbs end_ab
*Main> qs
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
*Main> s
(0,0)
*Main> fs
[(0,2),(1,0),(1,1),(1,2)]
*Main> d

*Main> (qs,s,fs,d) = catFSM oddbs end_ab
*Main> qs
[(0,[]),(0,[0]),(0,[0,1]),(0,[0,1,2]),(0,[0,2]),(0,[1]),(0,[1,2]),(0,[2]),(1,[]),(1,[0]),(1,[0,1]),(1,[0,1,2]),(1,[0,2]),(1,[1]),(1,[1,2]),(1,[2])]
*Main> s
(0,[])
*Main> fs
[(0,[0,1,2]),(0,[0,2]),(0,[1,2]),(0,[2]),(1,[0,1,2]),(1,[0,2]),(1,[1,2]),(1,[2])]
*Main>  

*Main> (qs,s,fs,d) = letterFSM 'a'      
*Main> qs
[0,1,2]
*Main> s
0
*Main> fs
[1]

*Main> (qs,s,fs,d) = reachable oddbs
*Main> qs
[0,1]
*Main> s
0
*Main> fs
[1]
-}