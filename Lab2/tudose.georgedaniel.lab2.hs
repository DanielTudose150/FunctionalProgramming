import Data.Char

{-
--Exercitiul 1

remove_uppercase :: [Char] -> [Char]
remove_uppercase [] = []
remove_uppercase (x:xs)
 | (ord x) >= (ord 'A') && (ord x) <= (ord 'Z') = remove_uppercase xs
 | otherwise = x:(remove_uppercase xs)



--Exercitiul 2

replace_lowercase :: [Char] -> [Char]
replace_lowercase [] = []
replace_lowercase (x:xs)
 | ( (ord x) >=(ord 'a') && (ord x) <= (ord 'z') ) = (toUpper x):(replace_lowercase xs)
 | otherwise = x:(replace_lowercase xs)



--Exercitiul 3
--head de lista vida da eroare => trebuie si cazul cu lista cu un singur element

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs)
 | (x > (head xs)) = False
 | otherwise = True && (isSorted xs)



-Exercitiul 4

is_in_list :: Int -> [Int] -> Bool
is_in_list x [] = False
is_in_list x [y] = (x==y)
is_in_list x (y:ys) 
 | (x==y) = True
 | otherwise = (is_in_list x ys)



--Exercitiul 5

count_occ :: Int -> [Int] -> Int
count_occ x [] = 0
count_occ x (y:ys)
 | (x==y) = 1 + (count_occ x ys)
 | otherwise = (count_occ x ys)


--Exercitiul 6
--Folosesc functia de la exercitiul 5
--Ca doua liste sa aiba aceleasi elemente, trebuie sa aiba acelasi numar de acelasi element

same_elements :: [Int] -> [Int] -> Bool
same_elements [] [] = True
same_elements [] ys = False
same_elements xs [] = False
same_elements xs ys
 | (count_occ (head xs) xs) == (count_occ (head xs) ys) = True && same_elements ([a | a <- xs, a/=(head xs)]) ([b | b <- ys, b/=(head xs)])
 | otherwise = False


--Exercitiul 7

-- presupunem ca apelul este ce_caut unde_caut
is_included_in :: [Int] -> [Int] -> Bool
is_included_in [] [] = True
is_included_in [] ys = True
is_included_in xs [] = False
is_included_in xs ys 
 | (count_occ (head xs) xs ) == (count_occ (head xs) ys ) = True && is_included_in ( [a | a <- xs, a/=(head xs)] ) ( [b | b <- ys, b/=(head xs)] )
 | otherwise = False



--Exercitiul 8

are_permutations :: [Int] -> [Int] -> Bool
are_permutations [] [] = True
are_permutations [] ys = False
are_permutations xs [] = False
are_permutations xs ys 
 | (same_elements xs ys) == False = False
 | otherwise = True



--Exercitiul 9
--obtine multiplii primului parametru
get_m :: Int -> [Int] -> [Int]
get_m x [] = []
get_m x ys = [ a | a <- ys, (mod a x) == 0]

--am folosit diferenta de liste din Data.List pentru a elimina multiplii lui x din restul listei
sieve :: [Int] -> [Int]
sieve [] = []
sieve [x] = [x]
sieve (x:xs) = x:(sieve (xs\\(get_m x xs)))











-}