-- CSci 119, Lab 4

import Data.List (sort, stripPrefix) -- for your solution to Lab 3
import Control.Monad (replicateM)    -- for strings function at the end


---------------- Code provided to you in Lab 3 ----------------

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

-- Include here any code you need from your solution to Lab 3 for testing
-- purposes. After a few days, I will release my solution for you to use.
-- Don't duplicate the code just given above.
-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL x xs) (LOL y ys) = (LOL (x+y) (xs++ys))

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb _ [] = False
memb x (y:ys) = case compare x y of
    LT -> False
    EQ -> True
    GT -> memb x ys

-- Merge of languages (aka "union"), preserves invariant
merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr) =
  case compare x y of
    LT -> x : merge xr ys
    EQ -> x : merge xr yr
    GT -> y : merge xs yr

-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] _ = []
cat _ [] = []
cat (x:xs) ys = merge (map (dot x) ys) (cat xs ys)     -- or   merge [dot x y | y <- ys] (cat xs ys)

lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let a) = lang [[a]]    -- or [lol [a]]   or  [LOL 1 [a]]
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2)
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(x,y) | x <- xs, y <- ys]

-- Powerset, preserves normalization. 
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq _ [] = []
leftq w@(LOL n xs) (LOL m ys:yss)
  | n <= m = case stripPrefix xs ys of
      Nothing -> leftq w yss
      Just zs -> LOL (m-n) zs : leftq w yss
  | otherwise = leftq w yss

-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct, sorted) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr [] = Star Empty
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Empty
finite [w] = onestr w
finite (w:ws) = Union (onestr w) (finite ws)
---------------- Part 1 ----------------

-- Implement the seven recursive predicates/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, unitary, byp, inf, rever, lq,
-- and nep. Each should begin with a type declaration. Include several tests
-- for each function.

emptiness:: RegExp -> Bool
emptiness Empty = True
emptiness (Let a) = False
emptiness (Union exr1 exr2) = emptiness(exr1) && emptiness(exr2)
emptiness (Cat exr1 exr2) = emptiness(exr1) || emptiness(exr2)
emptiness (Star a) = False 


unitary:: RegExp -> Bool
unitary Empty = False
unitary (Let a) = False
unitary (Union exr1 exr2) = (unitary (exr1) && emptiness(exr2)) || (emptiness(exr1) && unitary(exr2)) || (unitary(exr1) && unitary(exr2))
unitary (Cat exr1 exr2) = unitary (exr1) && unitary (exr2)
unitary (Star a) = emptiness (a) || unitary (a)


bybp:: RegExp -> Bool
bybp Empty = False
bybp (Let a) = False
bybp (Union exr1 exr2) = bybp (exr1) || bybp (exr2)
bybp (Cat exr1 exr2) = bybp (exr1) && bybp (exr2)
bybp (Star a) = True


infinite:: RegExp -> Bool
infinite Empty = False
infinite (Let a) = False
infinite (Union exr1 exr2) = infinite (exr1) || infinite (exr2)
infinite (Cat exr1 exr2) = (infinite (exr1) && not(emptiness (exr2))) || (infinite (exr2) && not(emptiness (exr1)))
infinite (Star a) = not(emptiness(a)) && not(unitary(a))


rev:: RegExp -> RegExp
rev Empty = Empty
rev (Let a) = Let a
rev (Union exr1 exr2) = Union (rev(exr1)) (rev(exr2))
rev (Cat exr1 exr2) = Cat (rev(exr1)) (rev(exr2))
rev (Star a) = Star (rev(a))


leqe:: Char -> RegExp -> RegExp
leqe s Empty = Empty
leqe s (Let a)
  | s == a = (Star(Empty))
  | otherwise = Empty
leqe s (Union exr1 exr2) = Union (leqe s exr1) (leqe s exr2)
leqe s (Cat exr1 exr2)
  | bybp (exr1) = Union (Cat (leqe s exr1) exr2) (leqe s exr2)
  | otherwise = Cat (leqe s exr1) exr2
leqe s (Star a) = Cat (leqe s a) (Star a)

npart:: RegExp -> RegExp
npart Empty = Empty
npart (Let a) = Let a
npart (Union exr1 exr2) = Union (npart exr1) (npart exr2)
npart (Cat exr1 exr2)
  | bybp (exr1) = Union (Cat (npart exr1) exr2) (npart exr2)
  | otherwise = Cat (npart exr1) exr2
npart (Star a) = Cat (npart a) (Star a)

------Part 1 Testing--------
----empty------
-- *Main> emptiness Empty
-- True
-- *Main> emptiness (Let 'a')
-- False
-- *Main> emptiness (Union (Let 'a') (Let 'b'))
-- False
-- *Main> emptiness (Cat (Let 'a') (Let 'b'))
-- False
-- *Main> emptiness (Cat (Empty) (Let 'a'))
-- 
----Unitary------
-- *Main> unitary Empty
-- False
-- *Main> unitary (Let 'a')
-- False
-- *Main> unitary (Union (Let 'a') (Let 'b'))
-- False
-- *Main> unitary (Cat (Let 'a') (Let 'b'))
-- False
-- *Main> unitary (Cat (Empty) (Let 'a'))
-- False

---bypassibility ------

-- *Main> bybp Empty
-- False  
-- *Main> bybp (Let 'a')
-- False  
-- *Main> bybp (Union (Let 'a') (Let 'b'))
-- False  
-- *Main> bybp (Cat (Let 'a') (Let 'b'))
-- False  
-- *Main> bybp (Cat (Empty) (Let 'a'))
-- False
-- *Main> bybp (Star (Let 'a'))
-- True

---infinitness-----
-- *Main> infinite Empty
-- False
-- *Main> infinite (Let 'a')
-- False
-- *Main> infinite (Star (Let 'a'))
-- True
-- *Main> infinite (Union (Empty) (Let 'a'))
-- False
-- *Main> infinite (Cat (Empty) (Let 'a'))
-- False

------rev-------
-- *Main> rev Empty
-- Empty
-- *Main> rev (Let 'a')
-- Let 'a'
-- *Main> rev (Union (Let 'a') (Let 'b'))
-- Union (Let 'a') (Let 'b')
-- *Main> rev (Star (Let 'a'))
-- Star (Let 'a')
-- *Main> rev (Cat (Let 'a') (Let 'b'))
-- Cat (Let 'a') (Let 'b')

---left quotient-----
-- *Main> leqe 'a' Empty
-- Empty
-- *Main> leqe 'a' (Let 'a')
-- Star Empty
-- *Main> leqe 'a' (Union (Let 'a') (Let 'b'))
-- Union (Star Empty) Empty
-- *Main> leqe 'a' (Cat (Empty) (Let 'a'))
-- Cat Empty (Let 'a')
-- *Main> leqe 'a' (Star (Let 'a'))
-- Cat (Star Empty) (Star (Let 'a'))


------nep------

-- *Main> npart Empty
-- Empty
-- *Main> npart (Let 'a')
-- Let 'a'
-- *Main> npart (Union (Let 'a') (Let 'b'))
-- Union (Let 'a') (Let 'b')
-- *Main> npart (Cat (Let 'a') (Let 'b'))
-- Cat (Let 'a') (Let 'b')
-- *Main> npart (Cat (Empty) (Let 'a'))
-- Cat Empty (Let 'a')
-- *Main> npart (Star (Let 'a'))
-- Cat (Let 'a') (Star (Let 'a'))



---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes,
-- where the second algorithm is the modified one I posted on Piazza (@96).
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits xs = [(take n xs, drop n xs) | n <- [0..length xs]]


match1 :: RegExp -> String -> Bool
match1 Empty w = False
match1 (Let a) w = w == [a]
match1 (Union exr1 exr2) w = match1 exr1 w || match1 exr2 w
match1 (Cat exr1 exr2) w = or [(match1 exr1 x) && (match1 exr2 y) | (x,y) <- splits w ]
match1 (Star a) w = (w == "") || or [(x /= "") && (match1 a x) && (match1 (Star a) y)| (x,y) <- splits w]



match2 :: RegExp -> String -> Bool
match2 r w = match2' (r:[]) w where
  match2':: [RegExp] -> String -> Bool
  match2' [] w = w == []
  match2' (Empty:rs) w = False
  match2' ((Let a):rs) w = (Let a == toRE[head w]) && match2' rs (tail w)
  match2' ((Union exr1 exr2):rs) w = match2' (exr1:rs) w || match2' (exr2:rs) w
  match2' ((Cat exr1 exr2):rs) w = match2' (exr1:exr2:rs) w
  match2' ((Star a):rs) w = match2' rs w || match2' (npart(Star a):(Star a):rs) w


-----part 2 testing -----
---splits----
-- *Main> splits "abc"
-- [("","abc"),("a","bc"),("ab","c"),("abc","")]
-- *Main> splits "ab"
-- [("","ab"),("a","b"),("ab","")]
-- *Main> splits "abcdef"
-- [("","abcdef"),("a","bcdef"),("ab","cdef"),("abc","def"),("abcd","ef"),("abcde","f"),("abcdef","")]

--match1----
-- *Main> match1 (Cat (Let 'a') (Let 'a')) "ab"
-- False
-- *Main> match1 Empty "a"
-- False
-- *Main> match1 (Let 'a') "a"
-- True
-- *Main> match1 (Union (Let 'a') (Let 'b')) "a"
-- True
-- *Main> match1 (Union (Let 'a') (Let 'b')) "b"
-- True
-- *Main> match1 (Star (Let 'a')) ""
-- True
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

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

