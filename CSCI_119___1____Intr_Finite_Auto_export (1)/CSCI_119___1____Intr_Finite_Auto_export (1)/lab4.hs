-- CSci 119, Lab 4

import Data.List (sort, stripPrefix) -- for your solution to Lab 3
import Control.Monad (replicateM)    -- for strings function at the end


-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

---- Regular expressions, along with input and output
data RegExp = Empty                -- Empty language
            | Let Char             -- Single letter language
            | Union RegExp RegExp  -- Union
            | Cat RegExp RegExp    -- Concatenation
            | Star RegExp          -- Kleene star
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

-- Quick and dirty postfix RegExp parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('@':xs) rs         = go xs (Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)


---------------- Your solution to Lab 3 ----------------

-- Include part of your solution to Lab 3 here for testing purposes.
-- After a few days, I will release my solution for you to use. Don't
-- duplicate the code just given above.
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(x,y)| x <-xs, y <- ys]

power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
                in [] : map (x:) ys ++ tail ys


dot :: LOL a -> LOL a -> LOL a
dot xs (LOL 0 _) = xs
dot (LOL x xs) (LOL y ys) = LOL (x+y) (xs++ys)

rev :: LOL a -> LOL a
rev (LOL x xs) = LOL x (reverse xs)

merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr) =
  case compare x y of
    LT -> x : merge xr ys
    EQ -> x : merge xr yr
    GT -> y : merge xs yr

cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] _ = []
cat _ [] = []
cat (x:xr) ys@(y:yr) = dot x y : merge (map (dot x) yr) (cat xr ys)

leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq _ [] = []
leftq w@(LOL n xs) (LOL m ys:yss) =
  case stripPrefix xs ys of
    Nothing -> leftq w yss
    Just zs -> LOL (m-n) zs : leftq w yss

-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let a) = lang [[a]]
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2)
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)

-- The one-string and finite languages of Theorem 3.2.
onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Empty
finite [w] = onestr w
finite (w:ws) = Union (onestr w) (finite ws)

kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr
kstar xs = eps : cat xs (kstar xs)

---------------- Part 1 ----------------

-- Membership for languages that satisfy the invariant (sorted, no duplicates),
-- even if they are infinite. Only look at the contents of a string when it is
-- necessary to do so, and stop as soon as you know the answer.
memb :: Ord a => LOL a -> Lang a -> Bool
memb (LOL a x) ((LOL b y):ys)
    | a < b = False
    | (a == b)  =  if (x == y) then True  else if (x< y) then False else memb (LOL a x) ys
    | (a>b) = memb (LOL a x) ys


-- Implement the seven recursive predications/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, unitary, byp, inf, rever, lq,
empty :: RegExp -> Bool
empty rs 
    | as == [] = True 
    | otherwise = False
    where as = lang_of rs

unitary :: RegExp -> Bool
unitary Empty = True 
unitary (Let a) = a == 'a' -- edit this
unitary (Union r1 r2) = unitary r1 && unitary r2
unitary (Cat r1 r2) = (unitary r1 && unitary r2) || empty r1 || empty r2
unitary (Star r1) = unitary r1

byp :: RegExp -> Bool -- fix this
byp (Empty) = False
byp (Let a)
    | lang_of (Let a) == [eps] = True
byp (Union r1 r2) = byp r1 || byp r2
byp (Cat r1 r2) = byp r1 && byp r2
byp (Star r) = True
{-byp rs
    | take 1 (lang_of rs) == [eps] = True
    | otherwise = False
-}
inf :: RegExp -> Bool
inf (Star r) = True
inf Empty = False
inf (Let a) = False
inf (Union r1 r2) = inf r1 || inf r2
inf (Cat r1 r2) = inf r1 || inf r2

rever :: RegExp -> RegExp
rever Empty = Empty
rever (Let a) = (Let a)
rever (Union r1 r2) = (Union (rever r1) (rever r2))
rever (Cat r1 r2) = (Cat (rever r2) (rever r1))
rever (Star r) = Star (rever(r))

lq :: Char -> RegExp -> RegExp
lq s Empty = Empty
lq s (Let a) 
    | s == a = Star Empty
    | otherwise = Empty
lq s (Union r1 r2) = (Union (lq s r1) ( lq s r2))
lq s (Cat r1 r2) 
    | byp r1 = (Union (Cat (lq s r1) r2)  (lq s r2))
    |otherwise = (Cat (lq s r1) r2)
lq s (Star r) = (Cat (lq s r) (Star r))


nep :: RegExp -> RegExp 
nep (Empty) = Empty
nep (Let a) = (Let a)
nep (Union r1 r2) = (Union (nep r1) (nep r2))
nep (Cat r1 r2) 
    | byp r1 = (Union (Cat (nep r1) r2) (nep r2))
    | otherwise = (Cat (nep r1) r2)
nep (Star r) = (Cat (nep r) (Star r))

 
--PART 1 testing:
memb_test = memb (LOL 2 "ab") [(LOL 1 "a"),(LOL 2 "ab")]

empty_test = empty ab

unitary_test = unitary ab

byp_test = byp ttla

inf_test = inf ttla

rever_test = Compact$ rever ttla

lq_test = Compact $ lq 'a' ab

nep_test = Compact $nep ab
--PART 1 testing end


---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes.
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits [] = [([],[])]
splits (xs) = [(splitAt a xs)| a <-[0..(length xs)]]-- ++ split xs

match1 :: RegExp -> String -> Bool
match1 (Empty) s = False

match1 (Let a) s
    | (Let a) == (onestr s) = True
    | otherwise = False
match1 (Union r1 r2) s = match1 r1 s || match1 r2 s
match1 (Cat r1 r2) s = (match1 r1 s) && (match1 r2 s)
match1 (Star r) (s:ss) 
    | s == ' '= True
    | (s /= ' ') && match1 r ss = True 
    | otherwise = False
--i tried
match2 :: RegExp -> String -> Bool
match2 r w = undefined
-- either empty or splits
-- uses a helper function
-- uses where, w1 != 0


-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get). 

sigma = ['a', 'b']                -- Alphabet used in all examples below

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once


-- For your tests, you may also find the following helpful. For example,
-- you can generate all strings of length 10 (or 20) or less and then test
-- to see whether match1 r w == memb (lol w) (lang_of r) for each such w.

--testing
splits_test = splits "Lab4"

match1_test = match1 ab sigma


-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

