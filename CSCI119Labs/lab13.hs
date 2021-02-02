import Data.List (foldl')
import Data.Char (isUpper)

-- CFG' G = (Start, Follows, Nullable)
type CFG' = (Char, Char -> Char -> [String], Char -> Bool)

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal


accept' :: CFG' -> [Char] -> Bool
accept' cfg@(s,d,e) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) es where
  lq a [] = []
  lq a (x:xs) | isUpper x = close (x:xs) a cfg
              | otherwise = if a == x then [xs] else [] -- terminal              
  es | e s = [[s],[]]
     | otherwise = [[s]]


close :: String -> Char -> CFG' -> [String]
close "" a cfg = []
close [x] a (s,d,e) | isUpper x = d x a
                    | otherwise = if a == x then [[]] else []
close (x:xs) a (s,d,e) | isUpper x, e x = (map (++ xs) (d x a)) ++ (close xs a (s,d,e))
                       | isUpper x = map (++xs) (d x a)
                       | otherwise = if a == x then [xs] else []

-- Balanced parentheses
-- Original:  S --> (S) | SS | ε
-- TNF + ε:   S --> (S) | (S)S  (S nullable)

bp' :: CFG'
bp' = ('S', d, e) where
  d 'S' '(' = ["S)", "S)S"]
  d 'S' ')' = []
  e 'S' = True

-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []

g5 :: CFG --Orignal S -> aSb | bSa | SS | E
g5 = ('S', d) where
    d 'S' 'a' = ["bS","aS","b","aSbS","bSaS"] ;    
    d 'S' 'b' = ["bS","aS","a","aSbS","bSaS"] ; 

--in TNG + ε S -> 
g5' :: CFG' 
g5' = ('S', d, e) where
    d 'S' 'a' = ["bS","aS","b","aSbS","bSaS"] ;    
    d 'S' 'b' = ["bS","aS","a","aSbS","bSaS"] ; 
    e 'S' = True

--TEsting
-- *Main> accept' bp' "()()"
-- True
-- *Main> accept' bp' "((()))"
-- True
-- *Main> accept' bp' "(("
-- False
-- *Main> accept' bp' "())"
-- False
-- *Main> accept' bp' "((((())))()"
-- False
-- *Main> accept' g5' "aabb"
-- False
-- *Main> accept' g5' "abab"
-- True
-- *Main> accept' g5' "baabba"
-- True
-- *Main> accept' g5' "abaaabbb"
-- False
-- *Main> accept' g5' "ab"
-- True

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