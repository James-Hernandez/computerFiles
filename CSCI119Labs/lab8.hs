-- Lab 8 Solution: Additional constructions, nondeterministic machines

import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)


---------------- Given functions ----------------

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

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

-- delta* construction
star :: (a -> Char -> a) -> (a -> [Char] -> a)
star = foldl'

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
             deriving (Show, Eq)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance (Show Compact) where    -- use precedence to minimize parentheses
  showsPrec d (Compact r) = sp d r where
    sp d Empty         = showString "@"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
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


---------------- Part 1: Additional constructions ----------------
-- Define the operations given in Section 7 of the notes

-- Intersection
inters :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a,b)
inters m1@(qs1,s1,fs1,d1) m2@(qs2,s2,fs2,d2) = (q,s,f,d) where
  q = qs1 >< qs2
  s = (s1,s2)
  f = fs1 >< fs2
  d ((q1, q2)) a = ((d1 q1 a),(d2 q2 a))
 

-- Complement
compl :: Eq a => FSM a -> FSM a
compl m1@(qs1,s1,fs1,d1) = (q,s,f,d) where
  q = qs1
  s = s1
  f = [qs' | qs' <- qs1, qs' `notElem` fs1]
  d = d1

homo_dir_helper:: (Char -> String)
homo_dir_helper 'a' = "aba"
homo_dir_helper 'b' = "bba"


homo_dir_letter:: String -> RegExp
homo_dir_letter [] = Star Empty
homo_dir_letter [a] = Let a
homo_dir_letter (a:w) = Cat (Let a) (homo_dir_letter w)


-- Direct homomorphic image: k is a substitution
homo_dir :: (Char -> String) -> RegExp -> RegExp
homo_dir k (Empty) = Empty
homo_dir k (Let a) = homo_dir_letter(k a)
homo_dir k (Union r1 r2) = Union (homo_dir k r1) (homo_dir k r2)
homo_dir k (Cat r1 r2) = Cat(homo_dir k r1) (homo_dir k r2)
homo_dir k (Star r1) = Star(homo_dir k r1)

-- Inverse homomorphic image
homo_inv :: Eq a => (Char -> String) -> FSM a -> FSM a
homo_inv k m1@(qs1,s1,fs1,d1) = (q,s,f,d) where
  q = qs1
  s = s1
  f = fs1
  d q1 a = star d1 q1 (k a)


-- Right quotient by a finite language
quot_right :: Eq a => [String] -> FSM a -> FSM a
quot_right w m1@(qs1,s1,fs1,d1)= (q,s,f,d) where
  q = qs1
  s = s1
  f = [q1 | q1 <- qs1, or[(star d1 q1 w1) `elem` fs1 | w1 <- w ]]
  d = d1

{-
Testing part1
*Main> (qs,s,fs,d) = inters oddbs end_ab
*Main> qs
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
*Main> s
(0,0)  
*Main> fs
[(1,2)]
*Main> (qs,s,fs,d) = compl oddbs       
*Main> qs
[0,1]
*Main> s
0
*Main> fs
[0]
*Main> (qs,s,fs,d) = compl end_ab
*Main> qs
[0,1,2]
*Main> s
0
*Main> fs
[0,1]
*Main> homo_dir_letter []
Star Empty
*Main> homo_dir_letter ['a']
Let 'a'
*Main> homo_dir_letter "aba" 
Cat (Let 'a') (Cat (Let 'b') (Let 'a'))
*Main> homo_dir_letter "ababbaba"
Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'b') (Cat (Let 'a') (Cat (Let 'b') (Let 'a')))))))

*Main> (qs,s,fs,d) = quot_right ["a"]  oddbs
*Main> qs
[0,1]
*Main> s
0
*Main> fs
[1]
*Main> (qs,s,fs,d) = quot_right ["aba"] avoid_aab
*Main> qs
[0,1,2,3]
*Main> s
0
*Main> fs
[0]

-}
---------------- Part 2: Nondeterministic machines ----------------

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)

-- nap_hat d qs a == normalized list of all transitions possible from qs on a
nap_hat :: Ord a => Trans a -> [a] -> Char -> [a]
nap_hat d qs a = norm $ concat [d q1 a| q<-x,q1<-q] where 
  x = power qs

-- nap_hat_star d qs w == normalized list of transitions possible from qs on w
nap_hat_star :: Ord a => Trans a -> [a] -> [Char] -> [a]
nap_hat_star = star . nap_hat

-- nacc m w == m accepd the string w
nacc :: Ord a => NFSM a -> [Char] -> Bool
nacc m@(qs1,s1,fs1,d1) w = overlap (nap_hat_star d1 s1 w) fs1


-- Nondeterministic FSMs with epsilon moves, indexed by their type of state
-- M = (states, starts, finals, transitions, epsilon-moves)
type Eps a = [(a, a)]
type EFSM a = ([a], [a], [a], Trans a, Eps a)

-- Normalized epsilon closure of a set of states (Hint: use uclosure)
eclose :: Ord a => Eps a -> [a] -> [a]
eclose eps qs1 = uclosure qs1 f where
  f q1 = [eps2 | (eps1, eps2) <- eps, q1 == eps1]
  
-- eap_has d es qs a == eclosure of transitions possible from qs on a
eap_hat :: Ord a => (Trans a, Eps a) -> [a] -> Char -> [a]
eap_hat (d, es) qs a  = eclose es (nap_hat d qs a)

eacc :: Ord a => EFSM a -> [Char] -> Bool
eacc = undefined

{-
TEsting pt2.

-}


---------------- Part 3: Conversion between machines ----------------

-- Easy conversion from FSM to NFSM (given)
fsm_to_nfsm :: Eq a => FSM a -> NFSM a
fsm_to_nfsm  m1@(qs1,s1,fs1,d1)= (q,s,f,d) where
  q = qs1
  s = [s1]
  f = fs1
  d q a = [d1 q a] 


-- Conversion from NFSM to FSM by the "subset construction"
nfsm_to_fsm :: Ord a => NFSM a -> FSM [a]
nfsm_to_fsm fm@(qs1,s1,fs1,d1) = (q,s,f,d) where
  q = power qs1
  s = s1
  f = [fs | fs <- q, fs1 `overlap` fs]
  d x a = nap_hat d1 x a--[q1 | q1 <- x, or[(d1 q1 a1) `elem` fs1 | a1 <- a ]]

-- Similar conversion from EFSM to FSM using epsilon closure
efsm_to_fsm :: Ord a => EFSM a -> FSM [a]
efsm_to_fsm m1@(qs1,s1,fs1,d1,eps1) = (q,s,f,d) where
  q = power qs1
  s = eclose eps1 s1
  f = [q' | q' <- q , q' `overlap` fs1]
  d x a = undefined

{-
TESting
*Main> (qs,s,fs,d) = fsm_to_nfsm oddbs
*Main> qs
[0,1]
*Main> s
[0]
*Main> fs
[1]
*Main> (qs,s,fs,d) = fsm_to_nfsm avoid_aab
*Main> qs
[0,1,2,3]
*Main> s
[0]
*Main> fs
[0,1,2]

*Main> (qs,s,fs,d) = nfsm_to_fsm m
*Main> qs
[[],[0],[0,1],[0,1,2],[0,1,2,3],[0,1,3],[0,2],[0,2,3],[0,3],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
*Main> s
[0]
*Main> fs
[[0],[0,1],[0,1,2],[0,1,2,3],[0,1,3],[0,2],[0,2,3],[0,3],[1],[1,2],[1,2,3],[1,3],[2],[2,3]]

*Main> j = fsm_to_nfsm oddbs
*Main> (qs,s,fs,d) = nfsm_to_fsm j
*Main> qs
[[],[0],[0,1],[1]]
*Main> s
[0]
*Main> fs
[[0,1],[1]]

-}

-- Gives the delta* function (recursive in w)
dstar :: Eq a => FSM a -> a -> [Char] -> a
dstar (_, _, _, d) = foldl d
-- Machine acceptance, Definition 1 (via delta*)
accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 m@(qs, s, fs, d) w = dstar m s w `elem` fs


-- Define a machine that accepts exactly the strings with an odd number of 'b's
-- (Solution given for illustration)
oddbs :: FSM Int
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

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

{- Tests:

1. m and fsm_to_nfsm m accept the same strings
2. m and nfsm_to_fsm m accept the same strings
3. m and efsm_to_fsm m accept the same strings

-}

quot_right_test = accept1 (quot_right ["ab"] end_ab) "b"
--True
compl_test = accept1 (compl oddbs) ['a','b']
--FAlse
inters_Test = accept1(inters avoid_aab oddbs) ['a','b','a']
--ture

--Test for 1.
fsm_to_nfsm_Test = fsm_to_nfsm end_ab
nacc_test= nacc fsm_to_nfsm_Test ['a','b','a']
--FAlse
accept1_test = accept1 end_ab ['a','b','a']
--FAlse

--Test for 2.
nfsm_to_fsm_test = nfsm_to_fsm fsm_to_nfsm_Test
nacc1_test = nacc fsm_to_nfsm_Test ['a','b','a'] 
--false
accept1_test_nfsm = accept1 nfsm_to_fsm_test ['a','b','a']
--false

--Test for 3.