
-- FIXME: still need to format functions to sort and remove duplicates without using norm, sort, nub, etc.

-- CSci 119, Lab 3

-- See http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html
import Data.List (sort, stripPrefix)


---------------- General list functions

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(x,y) | x <- xs, y <- ys]

-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = map (x:) (power xs)++power xs


---------------- Length-ordered lists

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

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL x xs) (LOL y ys) = lol (xs ++ ys)

-- Reverse of LOLs, preserves invariant
rev :: LOL a -> LOL a
rev (LOL x xs) = LOL x (reverse xs)



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]


-- Constructor for languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

-- Merge of langages (aka "union")
merge :: Ord a => Lang a -> Lang a -> Lang a
--merge as bs = as ++ bs
merge a [] = a
merge [] b = b
merge (a:as) (b:bs) 
    | a < b =  a: (merge as (b:bs))
    | otherwise = b:(merge (a:as) bs )
--testing cat variables
 
-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] ys = ys
cat xs [] = xs
cat (a:t1) (b:t2) = (dot a b : (merge (cat [a] t2) (cat t1 (b:t2))))

-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr
kstar xs = eps : cat xs (kstar xs)

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq (LOL n (xs)) [] = []
leftq (LOL n xs) (LOL m y:ys) = case stripPrefix xs y of 
                    Nothing -> leftq (LOL n xs) ys
                    Just zs -> (LOL (m-n) zs):leftq (LOL n xs) ys


---- Regular expressions and the languages they denote
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
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8l
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


-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of (Empty) = []
lang_of (Let c) = [LOL 1 [c]]
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2)
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)



-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr [] = Star Empty
onestr xs
    | (length xs == 1) = Let (head xs)
    | otherwise        = Cat (onestr (take 1 xs)) (onestr (tail xs))

finite :: [String] -> RegExp
finite [] = Star Empty
finite [x] = onestr x
finite (x:xs) = Union (onestr x) (finite xs)


-- Test all of the above operations extensively!
a =lang ["a","ab"]
b = lang ["b","bb"]

test_cart = cart a b

test_power =norm $ power a

test_lol = lol a 

test_dot = dot (lol "a") (lol "b") 

test_rev = rev test_dot

test_merge = merge a b

test_cat = norm $ cat a b

test_kstar = take 20 (kstar test_cat)

test_onestr = onestr "hello"

test_lang_of = lang_of test_onestr

test_finite = ["hi","bye"]
