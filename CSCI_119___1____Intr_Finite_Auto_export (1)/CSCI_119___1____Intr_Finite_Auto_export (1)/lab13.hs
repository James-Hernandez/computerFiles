import Data.List (foldl')
import Data.Char (isUpper)

-- CFG G = (Start, Follows) OLD GRAMMAR
--type CFG = (Char, Char -> Char -> [String])

-- CFG G = (Start, Follows, Nullable)
type CFG = (Char, Char -> Char -> [String], Char -> Bool)

accept :: CFG -> [Char] -> Bool
accept (s,d,e) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) (close [s]) where
  lq a [] = []
  lq a (x:xs)
              | isUpper x  = map (++ xs) $ concatMap close(d x a)  -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal
  
--  close:: String -> [String]
  close [] = [] 
  close (x:xs) 
    | isUpper x && e x = [(x:xs)] ++ close xs
    | otherwise = [x:xs]     


-- Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d, e) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []
  e 'S' = True 

bp1 :: CFG
bp1 = ('S',d,e) where
    d 'S' '(' = [")","()S", "(S)", "(S)S"]
    d 'S' ')' = []
    e 'S' = True
{-
-- {a^i b^j c^{i+j} | i >= 0, j > 0}
-- original: S --> aSc | T
--           T --> bTc | bc
-- in TNF:   S --> aSc | bTc | bc
--           T --> bTc | bc
pl = ('S', d) where
  d 'S' 'a' = ["Sc"]  ;  d 'S' 'b' = ["Tc","c"]  ;  d 'S' 'c' = []
  d 'T' 'a' = []      ;  d 'T' 'b' = ["Tc","c"]  ;  d 'T' 'c' = []
-}

g5 :: CFG
g5 = ('S', d, e) where
 d 'S' 'a' = ["bS","aS","b","aSbS","bSaS"] ;    
 d 'S' 'b' = ["bS","aS","a","aSbS","bSaS"] ; 
 e 'S' = True

g51 :: CFG
g51 = ('S', d, e) where
    d 'S' 'a'= ["aSb","bSa","SS"];
    d 'S' 'b'=["bSa","aSb","SS"];
    e 'S' = True
{-
g6 :: CFG
g6 = ('S', d) where
 d 'S' 'a' = ["bS","bSa","aSba","a","b","aSb","aS",""]
 d 'S' 'b' = ["bS","bSa","aSba","a","b","aSb","aS",""]

g2 :: CFG
g2 = ('R', d) where
d 'R' 'a' = ["aF", "bF", "0F", "1F", "aF+R", "bF+R", "0F+R", "1F+R","a","b",""] ; 
d 'R' 'b' = ["aF", "bF", "0F", "1F", "aF+R", "bF+R", "0F+R", "1F+R","a","b",""] ;
    d 'F' 'a' = ["aS", "bS", "b",""] ; d 'F' 'b' = ["b","bFS", "aFS", "baFS", "aaFS", ""];
    d 'S' 'a' = ["aA", "bA", "", "abA", "bbA"] ; d 'S' 'b' = ["bS*", "aS*", "", "a", "b", "ba", "ab"] ;
    d 'A' 'a' = ["", "a", "b", "ba", "ab", "bAa", "aAa"] ;
-}

{-
TESTING:
 
*Main> accept bp "()"
True
*Main> accept bp1 "()"
True

*Main> accept bp1 "((()()()))"
False
*Main> accept bp "((()()()))"
True
*Main> 


*Main> accept g5 "ababa"
False
*Main> accept g51 "ababa"
False

*Main> accept g5 "ab"
True
*Main> accept g51 "ab"
False

Different output for same language.
    leads to beleive im setting up grammar incorrectly




*Main> accept g5 "ab"
True
*Main> accept g5 "abab"
True
*Main> accept g5 "baab"
True
*Main> accept g5 "abba"
True
*Main> accept g5 "abbaa"
False
*Main> accept g5 "abbab"
False
 -
 -
*Main> accept g6 "a"
True
*Main> accept g6 "aba"
True
*Main> accept g6 "ababbb"
True
*Main> accept g6 "ababbba"
True
*Main> accept g6 "ababbbaabababababa"
True
*Main> accept g6 "ababbbaabababababaaaaaaaaaaaaa"
True
*Main> accept g6 "ababbbaabababababaaaaaaaaaaaaabbbbbbbbbbbbbbbb"
True
-
-
-
-}
