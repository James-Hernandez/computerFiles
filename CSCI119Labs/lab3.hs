-- CSci 119, Lab 3

-- See https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
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
cart [] ys = []
cart xs [] = []
cart xs ys = [(x,y) | x <- xs, y <- ys]

-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
-- power [2,3] = [[],[2],[2,3],[3]]
--power :: [a] -> [[a]]
power [] = [[]]
power [x] = [[],[x]]
power (p:ow) = sort (map (p:) (power ow) ++ power ow)


---------------- Length-ordered lists

-- Length-Ordered Lists over "character type" a (aka "strings over a")
-- Invariant: In LOL n xs, n == length xs
-- Note that automatically-derived Ord instance correctly orders LOLs
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
dot (LOL x xs) (LOL y ys) = (LOL (x+y) (xs++ys))

concat' xs [] = xs
concat' [] ys = ys
concat' (x:xs) ys = x: concat' xs ys

-- Reverse of LOLs, preserves invariant
rev :: LOL a -> LOL a
rev (LOL y' (y:ys)) = (LOL y' (rev' ys ++ [y]))

rev':: [a] -> [a]
rev' [] = []
rev' [x] = [x]
rev' (x:xs) = rev' xs ++ [x]



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]


-- Constructor for languages, establishes invariant
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb (LOL x' xs) ys = or[(LOL x' xs) == h | h <- ys]

-- Merge of langages (aka "union")
merge :: Ord a => Lang a -> Lang a -> Lang a
merge [] [] = []
merge (x:xs) [] = (x:xs) ++ []
merge [] (y:ys) = [] ++ (y:ys)
merge (x:xs) (y:ys)
   | x > y = y : (merge (x:xs) ys)
   | x < y = x : (merge xs (y:ys))
   | otherwise = x : (merge xs ys)



-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] [] = []
cat (x:xs) [] = []
cat [] (y:ys) = []
cat (x:xs) (y:ys)
    | x > y = (map (dot y) (x:xs)) ++ cat (x:xs) ys
    | x < y = (map (dot x) (y:ys)) ++ cat (y:ys) xs
    | otherwise = (map (dot x) [x]) ++ cat xs ys


-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq (LOL x' xs) [] = []
leftq (LOL x' xs) ((LOL y' y):ys) = case stripPrefix xs y of Nothing -> leftq (LOL x' xs) ys
                                                             Just q -> (lol q) : leftq (LOL x' xs) ys


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


-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let exr) = lang [[exr]]
lang_of (Union exr1 exr2) = merge (lang_of exr1) (lang_of exr2)
lang_of (Cat exr1 exr2) = cat (lang_of exr1) (lang_of exr2)
lang_of (Star exr1) = kstar (lang_of exr1)


-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct, sorted) strings l, lang_of (finite l) == l.

--I know that a one string regular languages are comprised of single letter regular languages that are all concatinated together
--but i am having some trouble on the understanding to make this a recursive function. I understand the concept but not the implementation

onestr :: String -> RegExp
onestr [] = Empty
onestr (x:[]) = Let x
onestr (x:xs) = Cat (onestr [x]) (onestr xs)

--onestr (x:ys@(y:zs)) Cat x y onstr xs
--"abcd"

--onestr xs = toRE (take 1 xs) : toRE (onestr (drop 1 xs))
--onestr xs = toRE (take 1 xs) then do onestr (drop 1 xs)

finite :: [String] -> RegExp
finite [] = Empty
finite (x:[]) = onestr x 
finite (x:xs) = Union (finite [x]) (finite xs)


-- Test all of the above operations extensively!            
-- *Main> cart [1,2,3] [4,5,6]
--[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

-- *Main> power [1,2]
--[[],[1],[1,2],[2]]

-- *Main> dot (lol "abc") (lol "def")
--"abcdef"

-- *Main> rev (lol "abc")
--"cba"

-- *Main> memb (lol "abc") [(lol "abc")]
--True
-- *Main> l1 = lang ["abc"]
-- *Main> l2 = lang ["def"]
-- *Main> merge l1 l2
-- ["abc","def"]
-- *Main> cat l1 l2
-- ["abcdef"]
-- *Main> leftq (lol "abc") [lol "abc"]
--[""]