---- CSci 119, Fall 2019, Lab 2 ----
import Data.List (sort)
-- As in Lab 1, we will be working with the finite universe U = [1..8]
u = [1..8]


----- PART 1:  Relations on u -----

-- A relation, R, on U is a list of the ordered pairs of elements of U:
type Reln = [(Int,Int)]
              
-- For example, here are the < and <= relations
less_reln :: Reln
less_reln = [(i,j) | j <- [1..8], i <- [1..j-1]]

leq_reln :: Reln
leq_reln  = [(i,j) | j <- [1..8], i <- [1..j]]
            
-- and here is the relation of equivalence mod 3:
eqmod3_reln :: Reln
eqmod3_reln = [(i,j) | i <- [1..8], j <- [1..8], (j - i) `mod` 3 == 0]


-- Write a function refl that tests whether a relation is reflexive:
-- R is reflexive if: forall a, (a,a) in R
-- Example: [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)] is the
-- smallest reflexive relation over u. Anything that does not contain
-- these 8 elements is not reflexive
refl :: Reln -> Bool
refl rs = and[elem (a,a) rs| (a,b)<- rs] 

-- Write a function symm that tests whether a relation is symmetric:
-- R is symmetric if: forall a b, (a,b) in R -> (b,a) in R
-- Example: [(1,1), (1,2), (2,1)] is symmetric but [(1,1), (1,2)] is not.
symm :: Reln -> Bool
symm rs = and [elem (b,a) rs| (a,b)<-rs]

-- Write a function trans that tests whether a relation is transitive:
-- R is transistive if: forall a b c, (a,b) in R /\ (b,c) in R -> (a,c) in R
-- Example: [(1,2),(2,3),(1,3),(4,4)] is transitive but [(2,3),(3,2)] is not
trans :: Reln -> Bool
trans rs = and [((elem (a,b) rs) && (elem (b,c) rs)) <= elem (a,c)rs | (a,b)<-rs,(b,c)<-rs]

-- Use the functions above to check the less, leq, and eqmod3 relations for
-- relexivity, symmetry, and transitivity


-- For each of the 8 possible combinations of yes/no on reflexivity,
-- symmetry, and transitivity, find a MINIMAL relation on u that has exactly
-- that combination of properties. Add a test to check whether you got the
-- properties right. (I'll do the first one as an example.)

--DONE***
-- refl, symm, trans
rst :: Reln
rst = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rst_test = refl rst && symm rst && trans rst

--DONE***
-- refl, symm, not trans 
rst' :: Reln
rst' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8),(1,2),(2,4),(4,2),(2,1)]
rst'_test = refl rst' && symm rst' && not( trans rst') 


--DONE***
-- refl, not symm, trans
rs't :: Reln
rs't = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8),(1,3)]
rs't_test = refl rs't && not(symm rs't) && trans rs't

--DONE***
-- refl, not symm, not trans
rs't' :: Reln
rs't' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8),(1,4),(4,2)]
rs't'_test = refl rs't' && not(symm rs't') && not(trans rs't')

-- DONE according to lecture notes, maybe add condition on "refl"
-- not refl, symm, trans
r'st :: Reln
r'st = []
r'st_test =not(refl r'st) && symm r'st && trans r'st

--DONE***
-- not refl, symm, not trans
r'st' :: Reln
r'st' = [(1,4),(4,1)]
r'st'_test = not(refl r'st') && symm r'st' && not(trans r'st')

--DONE***
-- not refl, not symm, trans
r's't :: Reln
r's't = [(1,4)]
r's't_test = not(refl r's't) && not(symm r's't) && trans r's't

--DONE***
-- not refl, not symm, not trans
r's't' :: Reln
r's't' = [(1,2),(2,4)]
r's't'_test = not(refl r's't') && not(symm r's't') && not(trans r's't')


---- PART 2: Partitions of u -----

-- A partitition, P, of u is a list of blocks, which are lists of elements
-- of u, that satisfies certain these conditions:
-- nontrivial: forall X in P, exists x in U, x in X, or
--             {} not in P
-- total: forall x in U, exists X in P, x in X
-- disjoint: forall X,Y in P (exists a, a in X /\ a in Y) -> X = Y, or
--           forall X,Y in P, X /= Y -> X intersect Y = {}
-- For example, here is the partitition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]

--remove duplicates
remDups :: Eq a => [a] -> [a]
remDups [] = []
remDups (x: [])= [x]
remDups (x:y:xs)
    | x == y = remDups (y:xs)
    | otherwise = x : remDups (y:xs)
--sorted
--cleaned and organized
orgniz :: Ord a => [a] -> [a]
orgniz = remDups.sort
-- Write a function part that tests whether a list of lists is a partition of u
partHelp :: [[Int]] -> [Int]
partHelp [] = []
partHelp (b:bs) = [x| x<-b] ++ partHelp bs

part :: [[Int]] -> Bool
part bs 
    | sort(partHelp bs) /= u = False
    | elem []  bs = False
    | otherwise = True


-- Write a function eq2part that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u.
eq2part :: Reln -> [[Int]]
eq2part rs = orgniz [eqclass a | a <- u]
    where
eqclass a = orgniz[b | b<-u ]


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u.
part2eq :: [[Int]] -> Reln
part2eq (b:bs) = [(x,y) | x<-b , y<-b] ++ part2eq bs 

