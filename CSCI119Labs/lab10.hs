-- Lab 9: Derivative-based conversion from RegExp' to FSM
-- (Brzozowski Construction)

import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

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
toRE w = parse w [] where
  parse [] [r] = r
  parse ('+':xs) (r2:r1:rs) = parse xs (Union r1 r2:rs)
  parse ('.':xs) (r2:r1:rs) = parse xs (Cat r1 r2:rs)
  parse ('*':xs) (r:rs) = parse xs (Star r:rs)
  parse ('@':xs) rs = parse xs (Empty:rs)
  parse (x:xs) rs = parse xs (Let x:rs)

-- Extended regular expressions, including a name for One = Star Empty,
-- and arbitrary numbers of arguments for (associative) Union and Cat
data RegExp' = Zero | One | Let' Char |
               Union' [RegExp'] | Cat' [RegExp'] | Star' RegExp'
  deriving (Eq, Ord, Show)

-- Convert ordinary RegExps into extended RegExps
fromRE :: RegExp -> RegExp'
fromRE Empty = Zero
fromRE (Let c) = Let' c
fromRE (Union r1 r2) = Union' [fromRE r1, fromRE r2]
fromRE (Cat r1 r2) = Cat' [fromRE r1, fromRE r2]
fromRE (Star r1) = Star' (fromRE r1)

-- Convert extended RegExps into ordinary RegExps, eliminating Union' and Cat'
-- on lists of length < 2, and right-associating them on longer lists
fromRE' :: RegExp' -> RegExp
fromRE' Zero = Empty
fromRE' One = Star Empty
fromRE' (Let' c) = Let c
fromRE' (Union' []) = Empty
fromRE' (Union' [r]) = fromRE' r
fromRE' (Union' (r:rs)) = Union (fromRE' r) (fromRE' (Union' rs))
fromRE' (Cat' []) = Star Empty
fromRE' (Cat' [r]) = fromRE' r
fromRE' (Cat' (r:rs)) = Cat (fromRE' r) (fromRE' (Cat' rs))
fromRE' (Star' r) = Star (fromRE' r)

-- Basic postfix parser for RE', assuming binary + and ., including 0 and 1
toRE' :: String -> RegExp'
toRE' w = parse w [] where
  parse [] [r] = r
  parse ('0':xs) rs = parse xs (Zero:rs)
  parse ('1':xs) rs = parse xs (One:rs)
  parse ('+':xs) (r2:r1:rs) = parse xs (Union' [r1,r2]:rs)
  parse ('.':xs) (r2:r1:rs) = parse xs (Cat' [r1,r2]:rs)
  parse ('*':xs) (r:rs) = parse xs (Star' r:rs)
  parse (x:xs) rs = parse xs (Let' x:rs)


-- An extended regular expression simplifier
simp :: RegExp' -> RegExp'
simp Zero = Zero
simp One = One
simp (Let' c) = Let' c
simp (Union' rs) = union' $ flat_uni $ map simp rs
simp (Cat' rs) = union' $ flat_cat $ map simp rs
simp (Star' r) = star' $ simp r

-- Smart constructor for Union' that normalizes its arguments and
-- handles the empty and singleton cases
union' :: [RegExp'] -> RegExp'
union' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Union' rs

-- Smart constructor for Cat' that handles the empty and singleton cases
cat' :: [RegExp'] -> RegExp'
cat' [] = One
cat' [r] = r
cat' rs = Cat' rs

-- Smart constructor for Star' that handles the constant and Star' cases
star' :: RegExp' -> RegExp'
star' Zero = One
star' One = One
star' (Star' r) = star' r
star' r = Star' r

-- Flatten a list of arguments to Union'; assumes each argument is simplified
flat_uni :: [RegExp'] -> [RegExp']
flat_uni [] = []
flat_uni (Zero:rs) = flat_uni rs
flat_uni (Union' rs':rs) = rs' ++ flat_uni rs
flat_uni (r:rs) = r : flat_uni rs

-- Flatten a list of arguments to Cat', turning them into a list of arguments
-- to Union'; assumes each argument is simplified
flat_cat :: [RegExp'] -> [RegExp']
flat_cat rs = fc [] rs where
  -- fc (args already processed, in reverse order) (args still to be processed)
  fc :: [RegExp'] -> [RegExp'] -> [RegExp']
  fc pr [] = [cat' $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (One:rs) = fc pr rs
  fc pr (Cat' rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union' rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs


-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)

type Trans a = a -> Char -> [a]
type NFSM a = ([a], [a], [a], Trans a)
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

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d) where
  qs' = uclosure [s] (\q -> map (d q) sigma)
  fs' = filter (`elem` fs) qs'

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr


-- Bypassable for extended REs, computed directly by recursion.
byp :: RegExp' -> Bool
byp Zero = False
byp One = True
byp (Let' a) = False
byp (Union' rs) = or [ byp r' | r' <- rs ]
byp (Cat' rs) = and [byp r' | r' <- rs]
byp (Star' a) = True

-- Regular-expression derivatives (aka left quotients) on extended REs,
-- computed directly by recursion.
deriv :: Char -> RegExp' -> RegExp'
deriv a Zero = Zero
deriv a One = Zero
deriv a (Let' c) = if a == c then One else Zero
deriv a (Union' rs) = Union' (map (\r' -> deriv a r') rs)
deriv a (Cat' rs) = Union' (derivCat a rs) where
  derivCat :: Char -> [RegExp'] -> [RegExp']
  derivCat a [] = []
  derivCat a (r':rs) = ((Cat' ((deriv a r'):rs) ):(rs')) where
    rs' = if byp r' then derivCat a rs else []
deriv a (Star' a') = Cat' [deriv a a', (Star' a')]

-- Convert an RegExp' to an FSM using the derivative (Brzozowski) construction.
-- States are SIMPLIFIED extended REs.  Note: to construct all the states,
-- you will have to use another closure process.
conv :: RegExp' -> FSM RegExp'
conv r = (qs1,s1,fs1,d1) where
  qs1 = uclosure [s1] (\qs' -> [simp (deriv c qs') | c <-sigma])
  s1 = simp r
  fs1 = [q | q <- qs1, byp q]
  d1 c q = simp (deriv q c)

-- Intersection
inters :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a,b)
inters m1@(qs1,s1,fs1,d1) m2@(qs2,s2,fs2,d2) = (q,s,f,d) where
  q = qs1 >< qs2
  s = (s1,s2)
  f = fs1 >< fs2
  d ((q1, q2)) a = ((d1 q1 a),(d2 q2 a))
 
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

-- Complement
compl :: Eq a => FSM a -> FSM a
compl m1@(qs1,s1,fs1,d1) = (q,s,f,d) where
  q = qs1
  s = s1
  f = [qs' | qs' <- qs1, qs' `notElem` fs1]
  d = d1
----LAB 10 BEGINS HERE----
-- Boolean binary operation on FSMs. Examples:
-- binopFSM (||) m1 m2 computes union machine
-- binopFSM (&&) m1 m2 computes intersection machine
binopFSM :: (Eq a, Eq b) => (Bool -> Bool -> Bool) -> FSM a -> FSM b -> FSM (a,b)
binopFSM op m1@(qs1,s1,fs1,d1) m2@(qs2,s2,fs2,d2) = (qs,s,fs,d) where
    qs = qs1 >< qs2
    s = (s1,s2)
    fs = [(q1',q2') | q1' <- qs1, q2' <-qs2, op (q1' `elem` fs1) (q2' `elem` fs2)]
    d (qs1, qs2) a = (d1 qs1 a, d2 qs2 a)


-- Reverse FSM to a NFSM. Output machine accepts reverse of language of input machine.
reverseFSM :: Eq a => FSM a -> NFSM a
reverseFSM m1@(qs1,s1,fs1,d1) = (qs,s,fs,d) where
    qs = qs1
    s = fs1
    fs = [s1]
    d qs a = [q' | q' <- qs1, (d1 q' a) == qs]

-- Reachable states of a NFSM (similar to reachable but on NFSMs)
nreachable :: Ord a => NFSM a -> [a]
nreachable m1@(qs1,s1,fs1,d1) = (uclosure s1 fs) where
    fs qs = norm $ concat(map (d1 qs) sigma)

-- Minimize a FSM. Put all of the above together to compute the minimum machine for m
minimize :: Ord a => FSM a -> FSM [a]
minimize m1@(qs1,s1,fs1,d1) = (qs,s,fs,d) where
    qs = complment qs1 
    s = s' where 
        s' = [s1]--[s' | s' <- qs, elem s1 s']
    fs = undefined --[fs' | fs' <- qs, elem fs' fs1]
    d qs a = undefined --[q' | q' <- qs, d1 q' a]
    complment x = [[qs'] | qs' <- qs1, notElem qs' x]

-- Test. For example, make sure that the minimal machine agrees with the original machine
-- on lots of input strings. Try for multiple machines.

--used for testing functions
-- Gives the delta* function (recursive in w)
dstar :: Eq a => FSM a -> a -> [Char] -> a
dstar (_, _, _, d) = foldl d
-- Machine acceptance, Definition 1 (via delta*)
accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 m@(qs, s, fs, d) w = dstar m s w `elem` fs

machine1 = toRE' "ab.*"
machine2 = toRE' "ab*"
convTest1 = conv machine1
convTest2 = conv machine2
-- acceptMachine1 = accept1 convTest1 ['a','b']

--------TEsts-------------------
--for my tests i dont test minimize because i could not figure out of to fix an error 
--of expected types and acutal types
{-
*Main> (qs,s,fs,d) = binopFSM (||) oddbs avoid_aab
*Main> qs
[(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3)]
*Main> fs
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(1,3)]
*Main> s
(0,0)
*Main> (qs,s,fs,d) = binopFSM (&&) oddbs avoid_aab
*Main> qs
[(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3)]
*Main> fs
[(1,0),(1,1),(1,2)]
*Main> s
(0,0)

*Main> (qs,s,fs,d) = reverseFSM oddbs
*Main> qs
[0,1]
*Main> s
[1]
*Main> fs
[0]
*Main> (qs,s,fs,d) = reverseFSM avoid_aab
*Main> qs
[0,1,2,3]
*Main> s
[0,1,2]
*Main> fs
[0]

*Main> m = reverseFSM oddbs
*Main> nreachable m
[0,1]
*Main> m = reverseFSM avoid_aab
*Main> nreachable m
[0,1,2]
-}




