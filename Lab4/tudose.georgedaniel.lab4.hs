data MobileDevice = Smartphone Culori | Laptop Culori | Tablet Int String Culori deriving (Show)

-- Exercitiul 1.2
{-
*Main> Smartphone

<interactive>:6:1: error:
    * No instance for (Show MobileDevice) arising from a use of `print'
    * In a stmt of an interactive GHCi command: print it
*Main> :r
[1 of 1] Compiling Main             ( lab4.hs, interpreted )
Ok, one module loaded.
*Main> Smartphone
Smartphone

*Main> :t Tablet 12
Tablet 12 :: MobileDevice
*Main> :r
[1 of 1] Compiling Main             ( lab4.hs, interpreted )
Ok, one module loaded.
*Main> :t Tablet 12 "A"
Tablet 12 "A" :: MobileDevice
-}

-- Exercitiul 1.3

data Culori = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Show,Eq)

{-
*Main> Smartphone Red
Smartphone Red
*Main> :t (Smartphone Red)
(Smartphone Red) :: MobileDevice
-}

-- Exercitiul 1.4
colour :: MobileDevice -> Culori
{-
colour (Smartphone Red) = Red
colour (Smartphone Orange) = Orange
colour (Smartphone Yellow) = Yellow
colour (Smartphone Green) = Green
colour (Smartphone Blue) = Blue
colour (Smartphone Indigo) = Indigo
colour (Smartphone Violet) = Violet
-}
colour (Smartphone x)
 | x == Red = Red
 | x == Orange = Orange
 | x == Yellow = Yellow
 | x == Green = Green
 | x == Blue = Blue
 | x == Indigo = Indigo
 | x == Violet = Violet
colour (Laptop x)
 | x == Red = Red
 | x == Orange = Orange
 | x == Yellow = Yellow
 | x == Green = Green
 | x == Blue = Blue
 | x == Indigo = Indigo
 | x == Violet = Violet
colour (Tablet _ _ x) 
 | x == Red = Red
 | x == Orange = Orange
 | x == Yellow = Yellow
 | x == Green = Green
 | x == Blue = Blue
 | x == Indigo = Indigo
 | x == Violet = Violet

{-

*Main> colour (Smartphone Blue)
Blue

*Main> Laptop Yellow
Laptop Yellow

*Main> colour (Tablet 15 "c" Blue)
Blue

-}

-- Exercitiul 2.1

data Arb = Leaf (Maybe Integer) | Nod Integer Arb Arb deriving (Show,Eq)

-- Exercitiul 2.2

isBST :: Arb -> Bool
isBST (Leaf Nothing) = False
isBST (Leaf (Just _)) = True
isBST (Nod _ (Leaf Nothing) (Leaf Nothing)) = True
isBST (Nod x (Leaf (Just left)) (Leaf Nothing)) 
 | x >= left = True
 | otherwise = False
isBST (Nod x (Leaf Nothing) (Leaf (Just right))) 
 | x < right = True
 | otherwise = False
isBST (Nod x (Leaf (Just left)) (Leaf (Just right))) 
 | x >= left && x < right = True
 | otherwise = False
isBST (Nod x (Nod left lleft lright) (Leaf Nothing)) 
 | x < left = False
 | otherwise = isBST (Nod left lleft lright)
isBST (Nod x (Leaf Nothing) (Nod right rleft rright)) 
 | x > right = False
 | otherwise = isBST (Nod right rleft rright)
isBST (Nod x (Nod left lleft lright) (Leaf (Just right))) 
 | x >= right = False
 | x < left = False
 | otherwise = isBST (Nod left lleft lright)
isBST (Nod x (Leaf (Just left)) (Nod right rleft rright)) 
 | x < left = False
 | x >= right = False
 | otherwise = isBST (Nod right rleft rright)
isBST (Nod x (Nod left lleft lright) (Nod right rleft rright)) 
 | x < left = False
 | x >= right = False
 | otherwise = isBST (Nod left lleft lright) && isBST (Nod right rleft rright)

{-
*Main> isBST (Leaf Nothing)
False

*Main> isBST (Leaf (Just 2))
True

*Main> isBST (Nod 5 (Leaf Nothing) (Leaf Nothing)) 
True

*Main> isBST (Nod 5 (Leaf (Just 2)) (Leaf Nothing))
True

*Main> isBST (Nod 5 (Leaf Nothing) (Leaf (Just 7)))
True

*Main> isBST (Nod 5 (Leaf (Just 6)) (Leaf Nothing))
False

*Main> isBST (Nod 5 (Leaf Nothing) (Leaf (Just 2)))
False

*Main> isBST (Nod 5 (Leaf (Just 2)) (Leaf (Just 7)))
False

*Main> isBST (Nod 5 (Leaf (Just 2)) (Leaf (Just 7)))
True

isBST (Nod 5 (Nod 4 (Leaf Nothing) (Leaf Nothing)) (Leaf Nothing))
True

*Main> isBST (Nod 5 (Nod 4 (Leaf (Just 3)) (Leaf Nothing)) (Leaf Nothing))
True

*Main> isBST (Nod 5 (Nod 4 (Leaf (Just 10)) (Leaf Nothing)) (Leaf Nothing))
False  

*Main> isBST (Nod 5 (Leaf (Just 2)) (Nod 7 (Leaf (Just 6)) (Leaf (Just 10))))
True  

*Main> isBST (Nod 5 (Leaf (Just 2)) (Nod 7 (Leaf (Just 6)) (Leaf (Just 1)))) 
False 

*Main> isBST (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 
True
-}


-- Exercitiul 2.2
searchBST :: Arb -> Integer -> Bool
searchBST (Leaf Nothing) _ = False
searchBST (Leaf (Just x)) y 
 | x == y = True
 | otherwise = False
searchBST (Nod x left right) y 
 | x == y = True
 | x < y = searchBST right y
 | otherwise = searchBST left y

{-
*Main> searchBST (Leaf Nothing) 5
False
*Main> searchBST (Leaf (Just 3)) 5
False
*Main> searchBST (Leaf (Just 5)) 5
True
*Main> searchBST (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 7
True
*Main> searchBST (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 8
False
-}

-- Exercitiul 2.3


insertBST :: Arb -> Integer -> Arb
insertBST (Leaf Nothing) x = Leaf (Just x)
insertBST (Leaf (Just x)) y 
 | x >= y = Nod x (Leaf (Just y)) (Leaf Nothing)
 | otherwise = Nod x (Leaf Nothing) (Leaf (Just y))
insertBST (Nod x left right) y 
 | x >= y = Nod x (insertBST left y) right
 | otherwise = Nod x left (insertBST right y)

{-

*Main> insertBST (Leaf Nothing) 5
Leaf (Just 5)

*Main> insertBST (Leaf (Just 5)) 3
Nod 5 (Leaf (Just 3)) (Leaf Nothing)
*Main> insertBST (Leaf (Just 5)) 6
Nod 5 (Leaf Nothing) (Leaf (Just 6))

*Main> insertBST (Nod 10 (Nod 5 (Leaf (Just 2)) (Leaf (Just 4))) (Nod 25 (Leaf (Just 20)) (Leaf (Just 30)))) 100
Nod 10 (Nod 5 (Leaf (Just 2)) (Leaf (Just 4))) (Nod 25 (Leaf (Just 20)) (Nod 30 (Leaf Nothing) (Leaf (Just 100))))

*Main> insertBST (Nod 10 (Nod 5 (Leaf (Just 2)) (Leaf (Just 4))) (Nod 25 (Leaf (Just 20)) (Leaf (Just 30)))) 3
Nod 10 (Nod 5 (Nod 2 (Leaf Nothing) (Leaf (Just 3))) (Leaf (Just 4))) (Nod 25 (Leaf (Just 20)) (Leaf (Just 30)))
-}

-- Exercitiul 2.5

maxim :: Arb -> Maybe Integer
maxim (Leaf Nothing) = Nothing
maxim (Leaf x) = x
maxim (Nod x _ (Leaf Nothing)) = Just x
maxim (Nod x _ (Leaf (Just right))) = Just right
maxim (Nod x _ (Nod right rleft rright)) = maxim (Nod right rleft rright)

{-

*Main> maxim (Leaf Nothing)
Nothing

*Main> maxim (Leaf (Just 2))
Just 2

*Main> maxim (Nod 10 (Nod 5 (Leaf Nothing) (Leaf Nothing)) (Leaf Nothing))
Just 10

*Main> maxim (Nod 10 (Nod 5 (Leaf Nothing) (Leaf Nothing)) (Leaf (Just 20)))
Just 20

*Main> maxim (Nod 10 (Nod 5 (Leaf Nothing) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf (Just 100))))
Just 100
-}

minim :: Arb -> Maybe Integer
minim (Leaf Nothing) = Nothing
minim (Leaf x) = x
minim (Nod x (Leaf Nothing) _) = Just x
minim (Nod x (Leaf (Just left)) _ ) = Just left
minim (Nod x (Nod left lleft lright) _) = minim (Nod left lleft lright)

{-
*Main> minim (Leaf Nothing)                                                                                     
Nothing

*Main> minim (Leaf (Just 2))
Just 2

*Main> minim (Nod 10 (Nod 5 (Leaf Nothing) (Leaf Nothing)) (Leaf Nothing))  
Just 5

*Main> minim (Nod 10 (Nod 5 (Leaf (Just 1)) (Leaf Nothing)) (Leaf (Just 20)))
Just 1

*Main> minim (Nod 100 (Nod 50 (Nod 25 (Leaf (Just 10)) (Leaf Nothing)) (Leaf Nothing)) (Leaf Nothing))
Just 10
-}

-- Exercitiul 2.6

removeMax :: Arb -> Arb
removeMax (Leaf Nothing) = Leaf Nothing
removeMax (Leaf _) = Leaf Nothing
removeMax (Nod x left (Leaf Nothing)) = left
removeMax (Nod x left (Leaf (Just right))) = Nod x left (Leaf Nothing)
removeMax (Nod x left right) = Nod x left (removeMax right)

{-
*Main> removeMax (Leaf Nothing)
Leaf Nothing
*Main> removeMax (Leaf (Just 2))
Leaf Nothing
*Main> removeMax (Nod 5 (Leaf Nothing) (Leaf Nothing))
Leaf Nothing
*Main> removeMax (Nod 5 (Leaf Nothing) (Leaf (Just 6)))
Nod 5 (Leaf Nothing) (Leaf Nothing)
*Main> removeMax (Nod 5 (Leaf Nothing) (Nod 10 (Leaf Nothing) (Leaf (Just 100))))
Nod 5 (Leaf Nothing) (Nod 10 (Leaf Nothing) (Leaf Nothing))
*Main> removeMax (Nod 5 (Leaf Nothing) (Nod 10 (Leaf (Just 6)) (Leaf Nothing)))
Nod 5 (Leaf Nothing) (Leaf (Just 6))
-}

-- Exercitiul 2.7
getValue :: Maybe Integer -> Integer
getValue (Just a) = a

removeBST :: Arb -> Integer -> Arb
removeBST (Leaf Nothing) _ = Leaf Nothing
removeBST (Leaf (Just x)) y 
 | x == y = Leaf Nothing
 | otherwise = Leaf (Just x)
removeBST (Nod x left (Leaf Nothing)) y
 | x == y = left
 | otherwise = Nod x (removeBST left y) (Leaf Nothing)
removeBST (Nod x (Leaf Nothing) right) y
 | x == y = right
 | otherwise = Nod x (Leaf Nothing) (removeBST right y)
removeBST (Nod x left right) y
 | y > x = Nod x left (removeBST right y)
 | y < x = Nod x (removeBST left y) right
 | y == x = Nod (getValue (maxim left)) (removeMax left) right 

{-
*Main> removeBST (Leaf Nothing) 5
Leaf Nothing
*Main> removeBST (Leaf (Just 3)) 5
Leaf (Just 3)
*Main> removeBST (Leaf (Just 5)) 5
Leaf Nothing

*Main> removeBST  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 7
Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf Nothing)) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))

*Main> removeBST  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 20
Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Leaf Nothing))

*Main> removeBST  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 11
Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf Nothing) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))

 (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing))))
-}

-- Exercitiul 2.8
preOrder :: Arb -> [Integer]
preOrder (Leaf Nothing) = []
preOrder (Leaf (Just x)) = [x]
preOrder (Nod x left right) = [x] ++ preOrder left ++ preOrder right

inOrder :: Arb -> [Integer]
inOrder (Leaf Nothing) = []
inOrder (Leaf (Just x)) = [x]
inOrder (Nod x left right) = inOrder left ++ [x] ++ inOrder right

postOrder :: Arb -> [Integer]
postOrder (Leaf Nothing) = []
postOrder (Leaf (Just x)) = [x]
postOrder (Nod x left right) = postOrder left ++ postOrder right ++ [x]

{-
*Main> preOrder (Leaf Nothing)
[]
*Main> preOrder (Leaf (Just 5))
[5]
*Main> preOrder  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 
[10,6,3,4,7,15,13,11,20]

*Main> inOrder (Leaf Nothing)
[]
*Main> inOrder (Leaf (Just 5))
[5]
*Main> inOrder  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing))))  
[3,4,6,7,10,11,13,15,20]

*Main> postOrder  (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing)))) 
[4,3,7,6,11,13,20,15,10]
-}

-- Exercitiul 3.1
heightAVL :: Arb -> Integer
heightAVL (Leaf Nothing) = 0
heightAVL (Leaf (Just _)) = 0
heightAVL (Nod x (Leaf Nothing) (Leaf Nothing)) = 0
heightAVL (Nod x left right) = 1 + max (heightAVL left) (heightAVL right)

{-
*Main> heightAVL (Leaf Nothing)
0
*Main> heightAVL (Leaf (Just 5))
0
*Main> heightAVL (Nod 10 (Leaf Nothing) (Leaf Nothing))
0

-}

-- Exercitiul 3.2
isAVL :: Arb -> Bool
isAVL (Leaf Nothing) = False
isAVL (Leaf (Just x)) = True
isAVL (Nod x left right)
 | abs (heightAVL left - heightAVL right) > 1 = False
 | otherwise = True

{-
*Main> isAVL (Nod 10 (Nod 6 (Nod 3 (Leaf Nothing) (Leaf (Just 4))) (Leaf (Just 7))) (Nod 15 (Nod 13 (Leaf (Just 11)) (Leaf Nothing)) (Nod 20 (Leaf Nothing) (Leaf Nothing))))
True
*Main> isAVL (Nod 10 (Nod 9 (Nod 8 (Leaf (Just 7)) (Leaf Nothing)) (Leaf Nothing)) (Leaf Nothing))
False
-}

-- Exercitiul 3.3

rotateLeft :: Arb -> Arb
rotateLeft (Leaf Nothing) = Leaf Nothing
rotateLeft (Leaf (Just x)) = Leaf (Just x)
rotateLeft (Nod x (Leaf Nothing) (Leaf Nothing)) = Nod x (Leaf Nothing) (Leaf Nothing)
rotateLeft (Nod x (Leaf (Just left)) (Leaf Nothing)) = Nod x (Leaf (Just left)) (Leaf Nothing)
rotateLeft (Nod x (Leaf (Just left)) (Leaf (Just right))) = Nod x (Leaf (Just left)) (Leaf (Just right))
rotateLeft (Nod x (Leaf Nothing) (Leaf (Just right))) = Nod x (Leaf Nothing) (Leaf (Just right))
rotateLeft (Nod x left (Nod right rleft rright)) = Nod right (Nod x left rleft) rright

rotateRight :: Arb -> Arb
rotateRight (Leaf Nothing) = Leaf Nothing
rotateRight (Leaf (Just x)) = Leaf (Just x)
rotateRight (Nod x (Leaf Nothing) (Leaf Nothing)) = Nod x (Leaf Nothing) (Leaf Nothing)
rotateRight (Nod x (Leaf (Just left)) (Leaf Nothing)) = Nod x (Leaf (Just left)) (Leaf Nothing)
rotateRight (Nod x (Leaf (Just left)) (Leaf (Just right))) = Nod x (Leaf (Just left)) (Leaf (Just right))
rotateRight (Nod x (Leaf Nothing) (Leaf (Just right))) = Nod x (Leaf Nothing) (Leaf (Just right))
rotateRight (Nod x (Nod left lleft lright) right) = Nod left lleft (Nod x lright right)

{-
*Main> rotateLeft (Leaf Nothing)
Leaf Nothing
*Main> rotateLeft (Leaf (Just 5))
Leaf (Just 5)
*Main> rotateLeft (Nod 10 (Leaf Nothing) (Leaf Nothing))
Nod 10 (Leaf Nothing) (Leaf Nothing)
*Main> rotateLeft (Nod 10 (Leaf Nothing) (Nod 15 (Leaf Nothing) (Leaf (Just 20))))
Nod 15 (Nod 10 (Leaf Nothing) (Leaf Nothing)) (Leaf (Just 20))

*Main> rotateRight (Nod 25 (Nod 15 (Leaf (Just 10)) (Leaf Nothing)) (Leaf Nothing))
Nod 15 (Leaf (Just 10)) (Nod 25 (Leaf Nothing) (Leaf Nothing))
-}

-- Exercitiul 3.4

doubleRotateLeft :: Arb -> Arb
doubleRotateLeft (Leaf Nothing) = Leaf Nothing
doubleRotateLeft (Leaf (Just x)) = Leaf (Just x)
doubleRotateLeft (Nod x (Leaf Nothing) (Leaf Nothing)) = Nod x (Leaf Nothing) (Leaf Nothing)
doubleRotateLeft (Nod x (Leaf (Just left)) (Leaf Nothing)) = Nod x (Leaf (Just left)) (Leaf Nothing)
doubleRotateLeft (Nod x (Leaf (Just left)) (Leaf (Just right))) = Nod x (Leaf (Just left)) (Leaf (Just right))
doubleRotateLeft (Nod x (Leaf Nothing) (Leaf (Just right))) = Nod x (Leaf Nothing) (Leaf (Just right))
doubleRotateLeft (Nod x xl (Nod y yl (Nod z zl zr))) = Nod y (Nod x xl yl) (Nod z zl zr)

doubleRotateRight :: Arb -> Arb
doubleRotateRight (Leaf Nothing) = Leaf Nothing
doubleRotateRight (Leaf (Just x)) = Leaf (Just x)
doubleRotateRight (Nod x (Leaf Nothing) (Leaf Nothing)) = Nod x (Leaf Nothing) (Leaf Nothing)
doubleRotateRight (Nod x (Leaf (Just left)) (Leaf Nothing)) = Nod x (Leaf (Just left)) (Leaf Nothing)
doubleRotateRight (Nod x (Leaf (Just left)) (Leaf (Just right))) = Nod x (Leaf (Just left)) (Leaf (Just right))
doubleRotateRight (Nod x (Leaf Nothing) (Leaf (Just right))) = Nod x (Leaf Nothing) (Leaf (Just right))
doubleRotateRight (Nod x (Nod y (Nod z zl zr) yr) xr) = Nod y (Nod z zl zr) (Nod x yr xr)

-- Exercitiul 4.1

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add x Zero = x
add Zero x = x
add (Succ x) y = Succ (add x y)

convert :: Nat -> Integer
convert Zero = 0
convert (Succ x) = 1 + convert x

convert' :: Integer -> Nat
convert' 0 = Zero
convert' x = Succ (convert' (x-1))

convert'' :: Maybe Nat -> Nat
convert'' (Just x) = x

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult x (Succ Zero) = x
mult (Succ Zero) x = x
--mult x y = convert' (convert x * convert y )
mult (Succ x) y = add y (mult x y)

exp' :: Nat -> Nat -> Maybe Nat
exp' x Zero
 | x /= Zero = Just (convert' 1)
 | otherwise = Nothing
exp' x (Succ Zero) = Just x
--exp' x y = Just (convert' (convert x ^ convert y))
exp' x (Succ y) = Just (mult x (convert'' (exp' x y)))

comp :: Nat -> Nat -> Bool
comp x y = convert x < convert y

dif :: Nat -> Nat -> Maybe Nat
dif x Zero = Just x
dif (Succ x) (Succ y)
 | comp (Succ x) (Succ y) = Nothing
 | otherwise = dif x y

impartire :: Nat -> Nat -> Maybe Nat
impartire _ Zero = Nothing
impartire x (Succ Zero) = Just x
impartire (Succ x) (Succ y)
 | comp (Succ x) (Succ y) = Just Zero
 | convert (Succ x) == convert (Succ y) = Just (Succ Zero)
 | otherwise = Just (add (Succ Zero) (convert'' (impartire (convert'' (dif (Succ x) (Succ y))) (Succ y))))
 -- Just (add (Succ Zero) convert'' (impartire (convert'' (dif (Succ x) (Succ y))) (Succ y)))

rest :: Nat -> Nat -> Maybe Nat
rest _ Zero = Nothing
rest _ (Succ Zero) = Just Zero
rest x y
 | comp x y = Just x
 | otherwise = rest (convert'' (dif x y)) y

{-

*Main> add (Succ Zero) (Succ Zero)
Succ (Succ Zero)

*Main> convert (Succ (Succ Zero))
2

*Main> convert' 2
Succ (Succ Zero)

*Main> mult (convert' 2) (convert' 2)
Succ (Succ (Succ (Succ Zero)))

*Main> exp' (convert' 2) (convert' 3) 
Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

*Main> impartire (convert' 5) Zero
Nothing
*Main> impartire (convert' 5) (convert' 1)
Just (Succ (Succ (Succ (Succ (Succ Zero)))))
*Main> impartire (convert' 5) (convert' 2)
Just (Succ (Succ Zero))

*Main> rest (convert' 10) Zero
Nothing
*Main> rest (convert' 10) (convert' 1)
Just Zero
*Main> rest (convert' 10) (convert' 2)
Just Zero
*Main> rest (convert' 10) (convert' 3)
Just (Succ Zero)
-}

-- Exercitiul 5.1
data ErrorNat = Error | Val Nat deriving (Show)
dif' :: Nat -> Nat -> ErrorNat
dif' x Zero = Val x
dif' (Succ x) (Succ y)
 | comp x y = Error
 | otherwise = dif' x y

{-
*Main> dif' (convert' 10) (convert' 5)
Val (Succ (Succ (Succ (Succ (Succ Zero)))))

*Main> dif' (convert' 2) (convert' 5)
Error
-}

convert3 :: ErrorNat -> Nat
convert3 (Val x) = x

convert4 :: Nat -> ErrorNat
convert4 = Val

-- Exercitiul 5.2 

impartire' :: Nat -> Nat -> ErrorNat
impartire' _ Zero = Error
impartire' x (Succ Zero) = Val x
impartire' (Succ x) (Succ y)
 | comp (Succ x) (Succ y) = Val Zero
 | convert (Succ x) == convert (Succ y) = Val (Succ Zero)
 | otherwise = Val (add (Succ Zero) (convert3 (impartire' (convert3 (dif' (Succ x) (Succ y))) (Succ y))))

{-
impartire (Succ x) (Succ y)
 | comp (Succ x) (Succ y) = Just Zero
 | otherwise = Just (add (Succ Zero) (convert'' (impartire (convert'' (dif (Succ x) (Succ y))) (Succ y))))


*Main> impartire' (convert' 10) (convert' 0)
Error

*Main> impartire' (convert' 10) (convert' 2)
Val (Succ (Succ (Succ (Succ (Succ Zero)))))
-}

-- Exercitiul 6.1
data Lista = Void | Con Integer Lista deriving (Show, Eq)

-- Exercitiul 6.2
findInList :: Lista -> Integer -> Bool
findInList Void _ = False
findInList (Con x Void) y
 | x == y = True
 | otherwise = False
findInList (Con x list) y
 | x == y = True
 | otherwise = findInList list y

{-
*Main> findInList (Con 5 Void) 5
True
*Main> findInList (Con 5 Void) 6
False
*Main> findInList (Con 5 (Con 4 (Con 6 Void))) 6
True
-}

-- Exercitiul 6.3
addList :: Lista -> Integer -> Lista
addList Void x = Con x Void
addList (Con el list) value = Con el (addList list value)

{-

*Main> addList Void 5
Con 5 Void

*Main> addList (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 5
Con 5 (Con 4 (Con 3 (Con 2 (Con 5 Void))))

-}

-- Exercitiul 6.4
addFirstList :: Lista -> Integer -> Lista
addFirstList Void x = Con x Void
addFirstList (Con el list) value = Con value (Con el list)

{-

*Main> addFirstList Void 5
Con 5 Void
*Main> addFirstList (Con 4 (Con 3 Void)) 5
Con 5 (Con 4 (Con 3 Void))

-}

-- Exercitiul 6.5
findPos :: Lista -> Integer -> Maybe Integer
findPos Void _ = Nothing
findPos (Con x list) pos
 | pos == 0 = Just x
 | pos == 1 && list == Void = Nothing
 | otherwise = findPos list (pos-1)

{-
*Main> findPos (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 1
Just 4
*Main> findPos (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 0
Just 5
*Main> findPos (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 2
Just 3
-}

-- Exercitiul 6.6
maxList :: Lista -> Integer -> Maybe Integer
maxList Void _ = Nothing
maxList (Con x Void) max 
 | x > max = Just x
 | otherwise = Just max
maxList (Con x list) max
 | x > max = maxList list x
 | otherwise = maxList list max

{-

*Main> maxList (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 0
Just 5

-}

-- Exercitiul 6.7
minList :: Lista -> Maybe Integer
minList Void = Nothing
minList (Con x Void) = Just x
minList (Con x list) = 
        let mini = minList list in
            case mini of
                Nothing -> Just x
                Just mini -> Just (min x mini)


{-

*Main> minList (Con 5 (Con 4 (Con 3 (Con 2 Void))))  
Just 2

-}

-- Exercitiul 6.8
catList :: Lista -> Lista -> Lista
catList Void Void = Void
catList Void x = x
catList x Void = x
catList (Con x list) right = Con x (catList list right)

{-

*Main> catList Void Void
Void
*Main> catList Void (Con 5 Void)
Con 5 Void
*Main> catList (Con 4 Void) (Con 5 Void)
Con 4 (Con 5 Void)

-}

-- Exercitiul 6.9
convertList1 :: Lista -> [Integer]
convertList1 Void = []
convertList1 (Con x list) = x:(convertList1 list)

convertList2 :: [Integer] -> Lista
convertList2 [] = Void
convertList2 (x:xs) = Con x (convertList2 xs)

{-

*Main> convertList1 (Con 5 (Con 4 (Con 3 (Con 2 Void)))) 
[5,4,3,2]

*Main> convertList2 []
Void
*Main> convertList2 [1,2,3]
Con 1 (Con 2 (Con 3 Void))

-}