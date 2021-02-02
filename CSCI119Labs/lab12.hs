import Data.List (foldl')
import Data.Char (isUpper)

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal

-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []

-- Example 2: {a^i b^j c^{i+j} | i >= 0, j > 0}
-- original: S --> aSc | T
--           T --> bTc | bc
-- in TNF:   S --> aSc | bTc | bc
--           T --> bTc | bc
pl = ('S', d) where
  d 'S' 'a' = ["Sc"]  ;  d 'S' 'b' = ["Tc","c"]  ;  d 'S' 'c' = []
  d 'T' 'a' = []      ;  d 'T' 'b' = ["Tc","c"]  ;  d 'T' 'c' = []


g5 :: CFG --Orignal S -> aSb | bSa | SS | E
g5 = ('S', d) where
    d 'S' 'a' = ["bS","aS","b","aSbS","bSaS"] ;    
    d 'S' 'b' = ["bS","aS","a","aSbS","bSaS"] ; 

g6:: CFG --takes in any possible string of a's and b's
--TNF: S -> abS | abSa | aaSba | aa | ab | aaSb | aaS
--     S -> bbS | bbSa | baSba | ba | bb | baSb | baS
g6 = ('S', d) where
    d 'S' 'a' = ["bS","bSa","aSba","a","b","aSb","aS",""]
    d 'S' 'b' = ["bS","bSa","aSba","a","b","aSb","aS",""]

g2:: CFG 
g2 = ('S', d) where
    d 'S' 'a' = ["aR", "bR", "0R", "1R", "aR+S", "bR+S", "0R+S", "1R+S","a","b",""];
    d 'S' 'b' = ["aR", "bR", "0R", "1R", "aR+S", "bR+S", "0R+S", "1R+S","a","b",""];
    d 'R' 'a' = ["aF", "bF", "b",""] ;d 'R' 'b' = ["b","bDF", "aDF", "baDF", "aaDF", ""];
    d 'F' 'a' = ["aA", "bA", "", "abA", "bbA"]; d 'F' 'b' = ["bF*", "aF*", "", "a", "b", "ba", "ab"] ;
    d 'A' 'a' = ["", "a", "b", "ba", "ab", "bAa", "aAa"] ;
    
-- Testing:
-- *Main> accept g5 "aa"
-- False
-- *Main> accept g5 "ab"
-- True
-- *Main> accept g5 "abbaab"
-- True
-- *Main> accept g5 "baba"
-- True
-- *Main> accept g5 "baabba"
-- True
-- *Main> accept g6 "aaa"
-- True
-- *Main> accept g6 "ababa"
-- True
-- *Main> accept g6 "bbbbb"
-- True
-- *Main> accept g6 "babaaab"
-- True
-- *Main> accept g6 "babababababbabababa"
-- True
-- *Main> accept g6 "aababababaaabab"
-- True