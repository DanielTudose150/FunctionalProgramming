-- Exercitiul 1
and' :: Bool -> Bool -> Bool
and' False False = False
and' True False = False
and' False True = False
and' True True = True 

or' :: Bool -> Bool -> Bool
or' False False = False 
or' True False = True
or' False True = True
or' True True = True

not' :: Bool -> Bool
not' False = True 
not' True = False

nand' :: Bool -> Bool -> Bool
nand' False False = True 
nand' False True = True
nand' True False = True
nand' True True = False

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' False True = False
nor' True False = False
nor' True True = False

imp' :: Bool -> Bool -> Bool
imp' False False = True
imp' False True = True
imp' True False = False
imp' True True = True

dimp' :: Bool -> Bool -> Bool
dimp' False False = True
dimp' False True = False
dimp' True False = False
dimp' True True = True

-- Exercitiul 2
isPrime :: Integer -> Bool
isPrime n = not (hasDivisors n 2 (n-1))

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True
hasDivisors n a b = hasDivisors n (a+1) b

{-
*Main> isPrime 2
True
*Main> isPrime 4
False
-}

-- Exercitiul 3

gcdminus :: Integer -> Integer -> Integer
gcdminus 1 y = 1
gcdminus x 1 = 1
gcdminus x y 
 | x == y = x
 | x > y = gcdminus (x-y) y
 | x<y = gcdminus x (y-x)


{-
a,b
r= a%b;
while !r
{
    a=b;
    b=r;
    r=a%b;
}
raspuns = b

*Main> gcdrest 39 26
13
*Main> gcdrest 39 17
1
-}
gcdrest :: Integer -> Integer-> Integer
gcdrest a b
 | a < b = gcdrest b a
 | mod a b == 0 = b
 | otherwise = gcdrest b (mod a b)

{-
 Exercitiul 4

 Este posibila aplicarea unui optimizari pentru apelurile recursive in pozitie de coada.
 Se folosesc acumulatorii
-}

 -- Exercitiul 5
 {-
*Main> fibo 4
3
*Main> fibo 5
5
*Main> fibo 6
8
*Main> fibo 7
13
 -}
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a b = a
fiboaux n a b = fiboaux (n-1) b (a+b)

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

{-
Rationament

(A0) fibo 0 = 0
(A1) fibo 1 = 1
(An) fibo n = fibo (n-1) + fibo (n-2)

(B0) fiboaux 0 a b = a
(Bn) fiboaux n a b = fiboaux (n-1) b (a+b)

(Cn) fibo' n = fiboaux n 0 1

Dorim sa demonstram ca fibo n este echivalent functional cu fibo' n

Cazul de baza: fibo 0 = fibo' 0
fibo 0 =(A0)= 0
fibo' 0 = fiboaux 0 0 1 =(B0)= 0
Ambele functii obtin acelasi rezultat

Cazul general: P(n):  fibo n = fibo' n
Presupunem ca P(n) este adevarata si dorim sa aratam ca P(n+1) este adevarata.
P(n) adevarata => fibo n = fibo' n adevarata <=> fibo n = fiboaux n 0 1
P(n+1) : fibo (n+1) = fibo' (n+1)

fibo (n+1) = fibo (n+1-1) (n+1-2) = fibo n (n-1)
fibo' (n+1) = fiboaux (n+1) 0 1 

fibo' n = fiboaux n 0 1 = fiboaux (n-1) 1 (0+1) = fiboaux (n-1) 1 1 =
        = fiboaux (n-2) 1 (1+1) = fiboaux (n-2) 1 2 = 
        = fiboaux (n-3) 2 (1+2) = fiboaux (n-2) 2 3 = ...
        = ... =
        = fiboaux 0 (fibo n) (fibo (n+1)) = fibo n

Constructia lui fibo' n poate fi reinterpretata astfel
fibo' n = fiboaux n 0 1 = fiboaux n (fibo 0) (fibo 1) = fiboaux (n-1) (fibo 1) ((fibo 0) + (fibo 1)) = fiboaux (n-1) (fibo 1) (fibo 2)
Din forma de apelare, al doilea parametru va reprezenta al n-lea numar din sirul lui fibonacii

Am obtinut aceeasi cantitate => Putem concluziona ca fibo si fibo' sunt echivalent functionale

-}

-- Exercitiul 7
{-
*Main> succ' 0
1
*Main> succ' 1
2
*Main> succ' 100
101
*Main> succ' (-2)
-1
*Main> succ' (-1)
0

-}
succ' :: Integer -> Integer
succ' 0 = 1
succ' n
 | n < 0 = -1 + succ' (n+1)
 | n > 0 = 1 + succ' (n-1)

-- Exercitiul 8

{-

*Main> add' 2 5
7
*Main> add' 10 10
20


*Main> mul' 3 5 0
15
*Main> mul' 4 5 0
20
-}
add' :: Integer -> Integer -> Integer
add' 0 b = b
add' a b = add' (a-1) (succ' b)

mul' :: Integer -> Integer -> Integer -> Integer
mul' 0 b acc = 0
mul' 1 b acc = add' b acc
mul' a b acc = mul' (a-1) b (add' b acc) 

-- Exercitiul 9
{-

*Main> mod' 15 4
3
*Main> mod' 16 4
0

*Main> div' 3 2 0
1
*Main> div' 3 4 0
0
*Main> div' 12 4 0
3
*Main> div' 13 4 0
3
*Main> div' 14 4 0
3
*Main> div' 15 4 0
3
*Main> div' 16 4 0
4

-}
mod' :: Integer -> Integer -> Integer
mod' a 1 = 0
mod' a b 
 | a > b = mod' (a-b) b
 | a == b = 0
 | a < b = a

div' :: Integer -> Integer -> Integer -> Integer
div' a b acc 
 | a < b = acc
 | otherwise = div' (a-b) b (1+acc)