-- Exemplu laborator
mysum x y = x + y
mysum' x y z = x + y + z
-- \Exemplu laborator

id x = x
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

myMax3 :: Int -> Int -> Int -> Int
myMax3 x y z = if (myMax x y) <= z then z else x

myMax32 :: Int -> Int -> Int -> Int
myMax32 x y z = if x <= y then if y <= z then z else y else if x <= z then z else x

mySumL :: Int -> Int
mySumL x = if x <= 0 then 0 else x + mySumL(x-1)

myFib :: Int -> Int
myFib x = if x == 0 then 0 else if x == 1 then 1 else myFib(x-1) + myFib(x-2)

mygcd :: Int->Int->Int
mygcd x y = if (x==0 || y==0) then 0 else if (x==1 || y==1) then 1 else if x>y then (mygcd (x-y) y) else if x<y then (mygcd x (y-x)) else x

-- Exemplu laborator
main = do
    putStrLn (show (mysum 10 20) )
-- \Exemplu laborator


{-

Exercitiul 2.1

Prelude> 2
2 
Prelude> 2 + 3                                                                                                          
5                                                                                                                       
Prelude> 2 + 3 * 5                                                                                                      
17                                                                                                                      
Prelude> (2 + 3) * 5                                                                                                    
25                                                                                                                      
Prelude> 3/5                                                                                                            
0.6                                                                                                                     
Prelude> 45345345346536 * 54425523454534333                                                                             
2467944156711854340070394620488                                                                                         
Prelude> 3/0                                                                                                            
Infinity                                                                                                                
Prelude> True                                                                                                           
True                                                                                                                    
Prelude> False                                                                                                          
False                                                                                                                   
Prelude> True && False                                                                                                  
False                                                                                                                   
Prelude> True || False                                                                                                  
True                                                                                                                    
Prelude> not True                                                                                                       
False                                                                                                                   
Prelude> 2 <= 3                                                                                                         
True                                                                                                                    
Prelude> not (2<=3)                                                                                                     
False                                                                                                                   
Prelude> (2<=3) || True                                                                                                 
True                                                                                                                    
Prelude> "aaa" == "aba"                                                                                                 
False                                                                                                                   
Prelude> "aba" == "aba"                                                                                                 
True                                                                                                                    
Prelude> "aaa" ++ "aba"                                                                                                 
"aaaaba"


Exercitiul 2.2
Prelude> 2                                                                                                              
2                                                                                                                       
Prelude> ( (+) 2 3)                                                                                                     
5 
Prelude> ( (+) 2 ((*) 3 5) )                                                                                            
17  
Prelude> ( (*) ( (+) 2 3) 5 )                                                                                           
25 
Prelude> ( (/) 3 5)                                                                                                     
0.6  
Prelude> ( (*) 45345345346536 54425523454534333)                                                                        
2467944156711854340070394620488 
Prelude> ( (/) 3 0 )                                                                                                    
Infinity  
Prelude> True                                                                                                           
True                                                                                                                    
Prelude> False                                                                                                         
False                                                                                                                   
Prelude> ( (&&) True False)                                                                                             
False                                                                                                                   
Prelude> ( (||) True False)                                                                                             
True  
Prelude> not True                                                                                                       
False  
Prelude> ( (not) ( (<=) 2 3 ) )                                                                                         
False  
Prelude> ( (<=) 2 3 )                                                                                                   
True
Prelude> ( (||) ( (<=) 2 3) True )                                                                                      
True
Prelude> ( (==) "aaa" "aba")                                                                                            
False                                                                                                                   
Prelude> ( (==) "aba" "aba")                                                                                            
True                                                                                                                    
Prelude> ( (++) "aaa" "aba")                                                                                            
"aaaaba"


Exercitiul 2.3
Prelude> :t True                                                                                                        
True :: Bool 
Prelude> :t False                                                                                                       
False :: Bool                                                                                                           
Prelude> :t True && False                                                                                               
True && False :: Bool                                                                                                   
Prelude> :t True && (2<=4)                                                                                              
True && (2<=4) :: Bool 



Exercitiul 2.4
Prelude> :t "aaa"                                                                                                       
"aaa" :: [Char]  




Exercitiul 2.5
Prelude> :t 2                                                                                                           
2 :: Num p => p                                                                                                         
Prelude> :t 2 + 3                                                                                                       
2 + 3 :: Num a => a                                                                                                     
Prelude> :t (+)                                                                                                         
(+) :: Num a => a -> a -> a   



Exercitiul 2.6
Prelude> not 2                                                                                                                                                                                                                                  
<interactive>:48:5: error:                                                                                                  
    * No instance for (Num Bool) arising from the literal `2'                                                               
    * In the first argument of `not', namely `2'                                                                              
    In the expression: not 2                                                                                                
    In an equation for `it': it = not 2 



Exercitiul 2.7
Prelude> :t not
not :: Bool -> Bool                                                                                                     
Prelude> :t 2                                                                                                           
2 :: Num p => p 

not primeste un argument care trebuie sa fie de tipul Bool, iar 2 este de tip numeric Num, deci not 2 va produce eroare



Exercitiul 3.1
Prelude> :t succ                                                                                                        
succ :: Enum a => a -> a 
Prelude> succ 3                                                                                                         
4   
Prelude> succ (2+3)                                                                                                     
6
Prelude> :t pred                                                                                                        
pred :: Enum a => a -> a                                                                                                
Prelude> pred 0                                                                                                         
-1
Prelude> :t max                                                                                                         
max :: Ord a => a -> a -> a                                                                                             
Prelude> max 1 10                                                                                                       
10
Prelude> max 2 (5-2)                                                                                                    
3
Prelude> :t min                                                                                                         
min :: Ord a => a -> a -> a                                                                                             
Prelude> min 5 3                                                                                                        
3



Exercitiul 3.2
Prelude> id x = x                                                                                                       
Prelude> id 6                                                                                                           
6 
Prelude> id (2+3)                                                                                                       
5 



Exercitiul 3.3
Prelude> sumThree x y z = x + y + z                                                                                     
Prelude> sumThree 10 12 11                                                                                              
33


Exercitiul 3.4
Prelude> mulThree x y z = x * y * z                                                                                     
Prelude> mulThree 1 2 3                                                                                                 
6 



Exercitiul 3.5
D:\WID\Proiecte\Programare functionala\Lab1>ghci test.hs                                                                
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help                                                         
[1 of 1] Compiling Main             ( test.hs, interpreted )                                                            
Ok, one module loaded.                                                                                                  
*Main>

D:\WID\Proiecte\Programare functionala\Lab1>ghci                                                                        
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help                                                         
Prelude> :l test.hs                                                                                                     
[1 of 1] Compiling Main             ( test.hs, interpreted )                                                            
Ok, one module loaded.                                                                                                  
*Main>

*Main> :r                                                                                                               
[1 of 1] Compiling Main             ( test.hs, interpreted )                                                            
Ok, one module loaded. 


Exercitiul 3.6

id x = x
sumThree x y z = x + y + z

Prelude> :l test.hs                                                                                                     
[1 of 1] Compiling Main             ( test.hs, interpreted )                                                            
Ok, one module loaded.
*Main> Main.id 6                                                                                                        
6                                                                                                                       
*Main> Main.sumThree 1 2 3                                                                                              
6  



Exercitiul 3.7
*Main> :t Main.sumThree                                                                                                 
Main.sumThree :: Num a => a -> a -> a -> a                                                                              
*Main> Main.sumThree 3.2 2 4                                                                                            
9.2  



Exercitiul 3.8

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

*Main> :t Main.sumThree                                                                                                 
Main.sumThree :: Int -> Int -> Int -> Int  

*Main> Main.sumThree 3.2 2 4                                                                                                                                                                                                                    
<interactive>:4:15: error:                                                                                                  
    * No instance for (Fractional Int) arising from the literal `3.2'                                                       
    * In the first argument of `sumThree', namely `3.2'                                                                       
    In the expression: sumThree 3.2 2 4                                                                                     
    In an equation for `it': it = sumThree 3.2 2 4 

myMax :: Int -> Int -> Int
myMax x y = if x <= y then y else x

Exercitiul 3.9
*Main> :t myMax                                                                                                         
myMax :: Int -> Int -> Int


Exercitiul 3.10
myMax3 :: Int -> Int -> Int -> Int
myMax3 x y z = if (myMax x y) <= z then z else x

myMax32 :: Int -> Int -> Int -> Int
myMax32 x y z = if x <= y then if y <= z then z else y else if x <= z then z else x


*Main> :r                                                                                                               
[1 of 1] Compiling Main             ( test.hs, interpreted )                                                            
Ok, one module loaded.                                                                                                  
*Main> myMax3 1 2 3                                                                                                     
3                                                                                                                       
*Main> myMax3 5 6 10                                                                                                    
10                                                                                                                      
*Main> myMax32 1 2 3                                                                                                    
3 


Exercitiul 3.11
*Main> mySumL 8                                                                                                         
36                                                                                                                      
*Main> mySumL 9                                                                                                        
45 


Exercitiul 3.12
myFib :: Int -> Int
myFib x = if x == 0 then 0 else if x == 1 then 1 else myFib(x-1) + myFib(x-2)
*Main> myFib 0                                                                                                          
0                                                                                                                       
*Main> myFib 1                                                                                                          
1                                                                                                                       
*Main> myFib 2                                                                                                          
1                                                                                                                       
*Main> myFib 3                                                                                                          
2                                                                                                                       
*Main> myFib 4                                                                                                          
3                                                                                                                       
*Main> myFib 10                                                                                                         
55                                                                                                                      
*Main> myFib 5                                                                                                          
5                                                                                                                       
*Main> myFib 6                                                                                                          
8 

Exercitiul 3.13
mygcd :: Int->Int->Int
mygcd x y = if (x==0 || y==0) then 0 else if (x==1 || y==1) then 1 else if x>y then (mygcd (x-y) y) else if x<y then (mygcd x (y-x)) else x


*Main> mygcd 14 6                                                                                                                 
2                                                                                                                                 
*Main> mygcd 18 4                                                                                                                 
2                                                                                                                                 
*Main> mygcd 21 24                                                                                                                
3                                                                                                                                 
*Main> mygcd 21 23                                                                                                                

1
-}

