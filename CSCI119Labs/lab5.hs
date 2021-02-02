-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2

import Data.List (isInfixOf, isSuffixOf)  -- useful for testing in Part 2

-- Again, for this (and most) labs, the alphabet will be {a,b} to keep testing
-- easier, but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d) with integer states
type FSM = ([Int], Int, [Int], Int -> Char -> Int)


---------------- Part 1: Representing FSMs

-- Check whether a finite state machine (qs, s, fs, d) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition function d gives a state in qs for every state in qs and
--     letter from sigma
unique:: [Int] -> Bool
unique [] = True
unique (x:xs) = not(elem x xs) && unique(xs)

isStartState:: Int -> [Int] -> Bool
isStartState x [] = False
isStartState x (y:ys) = x == y || (isStartState x ys)

isFinalState:: [Int] -> [Int] -> Bool
isFinalState final_S states_S = and [elem fs states_S | fs <- final_S]


isTransition:: (Int -> Char -> Int) -> [Int] -> [Char] -> Bool
isTransition del qs@(x:xs) sigmaa@(y:ys) = and [ (del q c) `elem` l | q <- qs, c <- sigmaa, l <- [qs]]


checkFSM :: FSM -> Bool
checkFSM (qs, s, fs, d) = (unique qs) && (isStartState s qs) && (isFinalState fs qs) && (isTransition d qs sigma)



delta:: FSM -> Int -> Char -> Int
delta (qs, s, fs, d) q a = if (a `elem` sigma) && (q `elem` qs) then d q a else q

-- Gives the delta* function (recursive in w)
dstar :: FSM -> Int -> [Char] -> Int
dstar m q "" = q
dstar m q (a:w) = dstar m (delta m q a) w

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 m@(qs, s, fs, d) [] = False
accept1 m@(qs, s, fs, d) w = (checkFSM m) && (dstar m s w `elem` fs)

-- Machine acceptance, Definition 2 (via L_q(M))
accept2 :: FSM -> [Char] -> Bool
accept2 m@(qs, s, fs, d) w = aux s w where
  -- aux q w = whether the machine, starting in q, accepts w (recursive in w)
  aux :: Int -> [Char] -> Bool
  aux q [] = (checkFSM m) && (q `elem` fs)
  aux q (w:ws) = aux (delta m q w) ws


---------------- Part 2: FSM construction

-- Define a machine that accepts exactly the strings with an odd number of b's
-- (Solution given for illustration)
oddbs :: FSM
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM
avoid_aab = ([0,1,2,3],0,[0,1,2],d) where
  d q 'a' = 1
  d q 'b' = q
  d 1 'b' = 0
  d 1 'a' = 2
  d 2 'a' = 2
  d 2 'b' = 3
  d 3 _ =  3

-- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM
end_ab = ([0,1,2],0,[2],d) where
  d q 'a' = 1
  d q 'b' = q
  d 1 'a' = 1
  d 1 'b' = 2
  d 2 'a' = 1
  d 2 'b' = 0


-- Define a function that takes a string and returns a machine that accepts
-- exactly that string and nothing else (compare to 'onestr' from Lab 3)
-- Hint: the expression w !! i gives the i-th character of the string w
exactly :: String -> FSM
exactly "" = ([0],0,[0],undefined)
exactly w = ([0..length w],0,[length w], undefined)
--exactly (w:ws) = undefined


-- Test the machines above. Also, try out the exerices at the end of Section 3.2
-- in my notes as FSMs. Do they produce the same results as your RegExp's did?

-- Recursive reimplementation of strings function from Lab 4
strings :: Int -> [String]
strings n = concat [strs i | i <- [0..n]] where
  strs 0 = [""]
  strs n = [a:xs | a <- sigma, xs <- strs (n-1)]

s10 = strings 10  -- or larger, if you like
s7 = strings 7

oddbs_test = and [accept1 oddbs w == odd (num_bs w) | w <- s10] where
  num_bs w = sum (map (\x -> if x == 'b' then 1 else 0) w)

avoid_aab_test = and [accept1 avoid_aab w | w <- s7]  

----Tests
-- *Main> checkFSM oddbs
-- True
-- *Main> accept1 oddbs sigma
-- True
-- *Main> accept1 avoid_aab sigma
-- True
-- *Main> accept1 end_ab sigma
-- False
-- *Main> checkFSM end_ab
-- True
-- *Main> accept2 oddbs sigma
-- True
-- *Main> accept2 avoid_aab sigma
-- True
-- *Main> accept2 end_ab sigma
-- False
-- *Main> checkFSM end_ab
-- True
-- *Main> checkFSM avoid_aab
-- True
-- *Main> dstar oddbs 0 sigma
-- 1
-- *Main> dstar avoid_aab 0 sigma
-- 1
-- *Main> dstar end_ab 0 sigma
-- 1
-- *Main> avoid_aab_test
-- False
-- *Main> oddbs_test
-- True