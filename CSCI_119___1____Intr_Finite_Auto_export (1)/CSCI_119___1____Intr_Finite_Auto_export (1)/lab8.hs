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
onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

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

-- Unary set closure (where "set" = normalized list) WTF
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
inters (q1,s1,fs1,d1) (q2,s2,fs2,d2) = (q,s,f,t) where 
    q = q1 >< q2
    s = (s1,s2)
    f={-[(q1',q2')| q1'<-q1, q2'<-q2, (q1' `elem` fs1) ,q2' `elem` fs2]-}fs1 >< fs2
    t ((q', q'')) a = ((d1 q' a),(d2 q'' a))
    

-- Complement
compl :: Eq a => FSM a -> FSM a
compl  (q1,s1,fs1,d1) = (q,s,f,t) where 
    q = q1
    s = s1
    f = [q' | q'<-q1 , notElem q' fs1 ]
    t = d1

hdir :: (Char -> String)
hdir 'a' = "aba"
hdir 'b' = "bab"

-- Direct homomorphic image: k is a substitution
homo_dir :: (Char -> String) -> RegExp -> RegExp
homo_dir k (Empty) = Empty
homo_dir k (Let c) = onestr ( k c )
homo_dir k (Union r1 r2) = Union (homo_dir k r1) (homo_dir k r2)
homo_dir k (Cat r1 r2) = Cat (homo_dir k r1) (homo_dir k r2)
homo_dir k (Star r) = Star (homo_dir k r)

-- Inverse homomorphic image
homo_inv :: Eq a => (Char -> String) -> FSM a -> FSM a
homo_inv k (qs1,s1,fs,t1) = (q,s,f,t) where
    q = qs1
    s = s1
    f = fs
    t q a = star t1 q (k a)

-- Right quotient
quot_right :: Eq a => [String] -> FSM a -> FSM a
quot_right w (q1,s1,fs1,d1)= (q,s,f,d) where
    q = q1
    s = s1
    f = [q'|q'<-q1, or[ elem (star d1 q' w') fs1|w'<-w]]
    d = d1

---------------- Part 2: Nondeterministic machines ----------------

-- Nondeterministic FSMs, indexed by their type of state
-- All states are normalized and the output of d is normalized
-- M = (states, starts, finals, transitions)  
type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)

-- nap_hat d qs a == normalized list of all transitions possible from qs on a
nap_hat :: Ord a => Trans a -> [a] -> Char -> [a]
nap_hat d qs a = norm $ concat [d q' a| q<-x,q'<-q] where x = power qs

-- nap_hat_star d qs w == normalized list of transitions possible from qs on w
nap_hat_star :: Ord a => Trans a -> [a] -> [Char] -> [a]
nap_hat_star =star . nap_hat

-- nacc m w == m accepd the string w
nacc :: Ord a => NFSM a -> [Char] -> Bool
nacc (qs,ss,fs,t) cs = overlap (nap_hat_star t ss cs) fs


-- Nondeterministic FSMs with epsilon moves, indexed by their type of state
-- M = (states, starts, finals, transitions, epsilon-moves)
type Eps a = [(a, a)]
type EFSM a = ([a], [a], [a], Trans a, Eps a)

-- Normalized epsilon closure of a set of states (Hint: use uclosure)
eclose :: Ord a => Eps a -> [a] -> [a]
eclose eps qs = uclosure qs f where
    f q = [e2 | (e1, e2) <- eps, q == e1] 
  
-- eap_has d es qs a == eclosure of transitions possible from qs on a
eap_hat :: Ord a => (Trans a, Eps a) -> [a] -> Char -> [a]
eap_hat (d,es) qs a  = eclose es (nap_hat d qs a)

eap_hat_star :: Ord a => (Trans a, Eps a) -> [a] -> [Char] -> [a]
eap_hat_star = star . eap_hat
eacc :: Ord a => EFSM a -> [Char] -> Bool
eacc (qs, ss, fs, t, eps) xs  = overlap (eap_hat_star (t,eps) ss xs) fs--overlap (nap_hat_star t s_e xs) fs where
    --s_e = eclose eps ss


---------------- Part 3: Conversion between machines ----------------

-- Easy conversion from FSM to NFSM (given)
fsm_to_nfsm :: Eq a => FSM a -> NFSM a
fsm_to_nfsm (qs1, s1, fs, d1) = (qs, s, f, d) where
    qs = qs1
    s = [s1]
    f = fs
    d q a = [d1 q a] --because it now produces a list 



-- Conversion from NFSM to FSM by the "subset construction"
nfsm_to_fsm :: Ord a => NFSM a -> FSM [a]
nfsm_to_fsm (qs1, s1, fs1, ts1) = (qs, s, fs, ts) where 
    qs = power qs1
    s = s1
    fs = [x | x<-qs, overlap fs1 x]
    ts q a = nap_hat ts1 q a


-- Similar conversion from EFSM to FSM using epsilon closure
efsm_to_fsm :: Ord a => EFSM a -> FSM [a]
efsm_to_fsm (qs1, s1, fs1, d1, eps) = (qs, s, f, d) where
    qs = subsequences qs1
    s = eclose eps s1
    f = [q'| q'<-qs, overlap q' fs1]
    d q a = eap_hat (d1,eps) q a

--helpers
--
delta_star ::Eq a => FSM a -> a -> [Char] -> a
delta_star m@(qs, s, fs, ts) q w@(x:xs) = delta_star m (ts q x) xs
delta_star m q [] = q

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: Eq a =>FSM a-> [Char] -> Bool
accept1 m@(qs, s, fs, ts)  w 
    | delta_star m s w  `elem` fs = True
    | otherwise = False 

odd_bs :: FSM Int
odd_bs = ([0,1], 0, [1], d) where
  d q a = if a == 'b' then (q+1) `mod` 2 else q

emptyFSM :: FSM Int
emptyFSM  = (qs,s,fs,ts) where
    qs = [0]
    s = 0
    fs = []
    ts 0 _ = 0

even_as :: EFSM Int
even_as = ([0,1], [0], [0], t,[(0,0)]) where
  t 0 'a' = [1]
  t 1 'a' = [0]
  t q _ = [q]



--
--helpers end

homo_dir_test = Compact$ homo_dir hdir (Union (Let 'a') (Let 'b'))

quot_test = accept1 (quot_right ["ab"] odd_bs) ['b','a','b']

hinv_test = accept1 (homo_inv hdir odd_bs) ['a','b'] -- true
--hdir a = aba, b = bab ::== 3 b's 
compl_test = accept1 (compl odd_bs) ['a','b','b'] -- true,
inter_test = accept1 (inters odd_bs efsm2fsm_test) ['a','a','b'] -- true,

--1.
fsm2nfsm_test = fsm_to_nfsm odd_bs 

nacc_test= nacc fsm2nfsm_test ['a','b','a'] --true

accept_test = accept1 odd_bs ['a','b','a'] --also true
--2.
nfsm2fsm_test = nfsm_to_fsm fsm2nfsm_test

accept2_fsm_test =accept1 nfsm2fsm_test ['b','a'] --true

nacc2_test = nacc fsm2nfsm_test ['b','a']         --also true
--3.
efsm2fsm_test = efsm_to_fsm even_as

eacc_test = eacc even_as ['a','a','b'] -- true

accept3_e2fsm_test = accept1 efsm2fsm_test ['a','a','b'] --both true


{- Tests:

1. m and fsm_to_nfsm m accept the same strings
2. m and nfsm_to_fsm m accept the same strings
3. m and efsm_to_fsm m accept the same strings

-}
