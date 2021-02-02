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
--avoid_aab :: FSM a
avoid_aab = ([0..3], 0, [0..2],ts) where
    ts 0 'a' = 1
    ts 0 'b' = 0
    ts 1 'a' = 2
    ts 1 'b' = 0
    ts 2 'a' = 2
    ts 2 'b' = 3
    ts 3 'a' = 3
    ts 3 'b' = 3

--oddbs :: FSM a
oddbs = ([0..4], 0 ,[3],ts) where
    ts q 'a' = q
    ts 3 'b' = 4
    ts 4 'b' = 3
    ts q 'b' = q+1
---------------- A solution to Lab 5, ported to FSM a ------------------------
  
-- nodups xs = "xs has no duplicates"
nodups :: Eq a => [a] -> Bool
nodups [] = True           
nodups (x:xs) = notElem x xs && nodups xs

-- subset xs ys == "xs is a subset of ys"
subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys
-- OR: subset xs ys = all (`elem` ys) xs

-- func d qs == "d is a function from qs x sigma to qs"
func :: Eq a => (a -> Char -> a) -> [a] -> Bool
func d qs = and [elem (d q a) qs | q <- qs, a <- sigma]

-- check whether a finite state machine is correct/complete:
checkFSM :: Eq a => FSM a -> Bool
checkFSM (qs, s, fs, d) = nodups qs &&         -- (1)
                          elem s qs &&         -- (2)
                          subset fs qs &&      -- (3)
                          func d qs            -- (4)
                           
-- All functions below assume that the machine is correct

delta_star :: Eq a => FSM a -> a -> [Char] -> a
delta_star (_, _, _, d) = foldl d

accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 m@(_, s, fs, _) w = elem (delta_star m s w) fs

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m@(_, _, _, d) q (a:w) = accept2_aux m (d q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w

-- odd_bs is a machine that accepts strings with an odd number of b's
-- states: (number of b's read so far) mod 2
odd_bs :: FSM Int
odd_bs = ([0,1], 0, [1], d) where
  d q a = if a == 'b' then (q+1) `mod` 2 else q

-- avoid w is a machine that accepts strings that don't have w as a substring
-- states: prefixes of w read so far (with w itself as a trap state)
avoid :: String -> FSM String
avoid xs = (qs, "", init qs, d) where
  qs = inits xs
  d w a = if w == xs then w else head [w | w <- tails (w++[a]), isPrefixOf w xs]

-- no_aab is a machine that accepts strings that don't have "aab" as a substring
no_aab :: FSM String
no_aab = avoid "aab"

-- ends_in w is a machine that accepts strings that end in w
-- states: last <= n characters read (in reverse order, for ease of computation)
-- Note: this machine has 2^(n+1) - 1 states when |sigma| = 2
ends_in :: String -> FSM String
ends_in xs = (qs, "", [reverse xs], d) where
  n = length xs
  qs = strings n
  d w a = take n (a : w)

-- ends_in_ab is a machine that accepts strings ending in "ab"
ends_in_ab :: FSM String
ends_in_ab = ends_in "ab"


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
emptyFSM  = (qs,s,fs,ts) where
    qs = [0]
    s = 0
    fs = []
    ts 0 _ = 0

-- Machine that accepts the language {"a"} where a in sigma
letterFSM :: Char -> FSM Int
letterFSM a = (qs,s,fs,ts) where
    qs = [0..2]
    s = 0
    fs = [1]
    ts q c 
        | c == a = 1
        | otherwise = 2
 

-- Machine that accepts the union of the languages accepted by m1 and m2
unionFSM :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a, b)
unionFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1,s2)
  fs = [(q1,q2) | q1<-qs1, q2<-qs2, (q1 `elem` fs1) ,( q2 `elem` fs2) ]
  d (q1 ,q2) a = (d1 q1 a, d2 q2 a )-- one line loop using set comprehension


-- Machine that accepts the concatenation of the languages accepted by m1 and m2
-- uses powerset and correction
catFSM :: (Eq a, Ord b) => FSM a -> FSM b -> FSM (a, [b])
catFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  correct1 q d = union d [s2 | elem q fs1]
  qs = [(x,y) | x <-qs1, y <- power qs2, correct1 x y == y]
  s  = (s1, correct1 s1 [])
  fs = [(x,y)| (x,y)<-qs, overlap y fs2]
  d (q,x) a= (q', correct1  q' (dhat x a)) where
    q' = d1 q a
    dhat x' a = norm [d2 q2 a| q2<-x']

-- Machine that accepts the Kleene star of the language accepted by m1
-- uses powerset and correction
starFSM :: Ord a => FSM a -> FSM [a]
starFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
--correct1 q d = union d [s1 | elem q fs1]
  qs = [bx| bx <-power qs1, not(fs1 `subset` bx) ,not( s1 `elem` bx)]
  s  = []
  fs = [q1 | q1 <- qs, overlap q1 fs1]
  d [] a = norm [s1 | q `elem` fs1] ++ [q] where
    q = d1 s1 a
  d x a = norm [s1 | overlap y fs1] ++ y where
    y = map (\q -> d1 q a) x
--correction function is a helper function:: using map?

--
--Delta (q1,q2) a = (  )


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



--testing
--


test_union = accept1 (unionFSM oddbs avoid_aab) "aaba" 
test_union1 = accept1 (unionFSM oddbs avoid_aab) "baaba" 
test_cat = accept1 (catFSM oddbs avoid_aab) "b" 
test_cat1 = accept1 (catFSM oddbs avoid_aab) "a" 

test_star = accept1 (starFSM avoid_aab) "aab" 
test_star1 = accept1 (starFSM avoid_aab) "b" 
