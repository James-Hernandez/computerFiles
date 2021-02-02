-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2

-- Again, for this (and most) labs, the alphabet will be {a,b} to keep testing
-- easier, but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d) with integer states
-- d == transition , represents a function
-- Q = list of states , integers
type FSM = ([Int], Int, [Int], Int -> Char -> Int)

-- Check whether a finite state machine (qs, s, fs, ts) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition function gives a state in qs for every state in qs and
--     letter from sigma
one :: FSM
one = ([0,1], 0, [1], f) where
    f 1 _ = 1
    f q c = if (c == 'a') then 1 else 0
    



checkFSM :: FSM -> Bool
checkFSM (qs, s, fs, ts) 
    | not (dupCheck qs) = False
    | not (s `elem` qs) = False  
    | not (and [a `elem` qs| a <-fs]) = False
    | not(and [(ts a sig) `elem` qs  | a<-qs,sig <- sigma]) = False
    | otherwise = True

--check for duplicates
dupCheck :: [Int] -> Bool
dupCheck [] = True
dupCheck (x:xs) 
    | x `elem` xs = False
    | otherwise = dupCheck xs

-- USES DEF 4.1
-- helper function to check Q is finite
-- start state is within Q
-- Final state is within Q
-- subset function for Fs subset Qs
-- All inputs and outputs of ts are within Q
-- Gives the delta* function (recursive in w)



delta_star :: FSM -> Int -> [Char] -> Int
delta_star m@(qs, s, fs, ts) q w@(x:xs) = delta_star m (ts q x) xs
delta_star m q [] = q

-- ts q x until empty THE OUTPUT IS PASSED TO ACCEPT1
-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 m@(qs, s, fs, ts)  w 
    | delta_star m s w  `elem` fs = True
    | otherwise = False 
-- 

-- Machine acceptance, Definition 2 (via L_q(M))

-- accept2_aux m q w = whether m, starting in q, accepts w (recursive in w)
accept2_aux :: FSM -> Int -> [Char] -> Bool
accept2_aux m@(qs, s, fs, ts) q [] = elem q fs
accept2_aux m@(qs, s, fs, ts) q w@(x:xs)  
    | r `elem` fs = accept2_aux m r xs 
    | otherwise = False  
    where r = ts q x 

-- Acceptance, defined (non-recursively) in terms of accept2_aux
accept2 :: FSM -> [Char] -> Bool
accept2 m@(qs, s, fs, ts) w = accept2_aux m s w 


---- FSM construction

-- Define a machine that accepts exactly the strings with an odd number of b's
-- and test it adequately
oddbs :: FSM
oddbs = ([0..4],0,[3],ts) where 
    ts q 'a' = q
    ts 3 'b' = 4
    ts 4 'b' = 3
    ts q 'b' = q+1

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
avoid_aab :: FSM
avoid_aab = ([0..3],0,[0..2],ts) where
    ts 0 'a' = 1
    ts 0 'b' = 0
    ts 1 'a' = 2
    ts 1 'b' = 0
    ts 2 'a' = 2
    ts 2 'b' = 3
    ts 3 'a' = 3
    ts 3 'b' = 3

-- Define a machine that accepts all strings that end in "ab" and test
end_ab :: FSM
end_ab = ([0..2],0,[2],ts) where
    ts 0 'a' = 1
    ts 0 'b' = 0
    ts 1 'a' = 1
    ts 1 'b' = 2
    ts 2 'a' = 1
    ts 2 'b' = 0
-- Define a function that takes a string and returns a machine that excepts
-- exactly that string and nothing else
exactly :: String -> FSM --sorry this doesn't work
exactly s = (qs,s_0,fs,ts) where
    n = length(s)
    qs = [0..n]
    s_0 = 0
    fs = [n-1]
    ts q 'a' 
        | s !! (q+1) == 'a' = q+1
        | otherwise = q
    ts q 'b'
        | s !! (q+1) == 'b' = q+1
        | otherwise = q
    ts q _ = n


--testing
test_acc1_bst = accept1 oddbs ['a','b','b','b']
test_acc1_bsf = accept1 oddbs ['a','b','a','b']

test_acc2_oddbs = accept2 oddbs ['a','b','b','b'] -- should be true

test_acc2_avoid_aabf = accept2 avoid_aab ['a','a','b','b'] 
test_acc2_avoid_aabt = accept2 avoid_aab ['a','b','b','b'] 

test_acc2_end_abf = accept2 end_ab ['a','a','b','b'] 
test_acc1_end_abt = accept1 end_ab ['a','a','a','b'] 

test_checkFSM_oddbs = checkFSM oddbs
test_checkFSM_end_ab = checkFSM oddbs
test_checkFSM_avoid_aab = checkFSM oddbs


