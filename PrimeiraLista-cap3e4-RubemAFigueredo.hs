{- Rubem Alves Figueredo
Exercícios do Capítulo 3 e Capítulo 4 do livro texto:
HASKELL 
the craft of functional programming - there edition
By Simon Thompson
-}
-- ================================================

-- 3.1)
xor::Bool->Bool->String
Xor a b
   | a && b    = "False"
   | otherwise = "True"

-- ================================================

{--
3.2)
--}
-- ================================================

{--
3.3) exOr True True  = False
     exOr True False = True
     exOr False True = True
     ExOr False False= False
--}
-- ================================================

{--
3.4) && -> Na tabela verdade corresponde a, quando as duas entradas são verdadeiras o resultado será sempre verdadeira e se pelo menos umas das entradas for falsa a resposta será falsa.
     || -> Na tabela verdade corresponde a, quando pelo menos uma das entradas for verdadeira o resultado será verdadeiro, caso contrário, o resultado será falso.
--}
-- ================================================

-- 3.5)
nAnd1::Bool->Bool->Bool
nAnd a b
   | a && b = False
   | otherwise = True

nAnd2::Bool->Bool->Bool
nAnd2 a b
  | a && b = not(True)
  | otherwise = true

-- ================================================

{--
3.6) nAnd True True   = False
     nAnd True False  = True
     nAnd False True  = True
     nAnd False False = True

--}

{-- QuickCheck para o 3.5:
3.7) --}
prop_nAnd1::Bool->Bool->Bool
prop_nAnd1 a b = nAnd1 a b == False &&
                 nAnd1 a b == True

prop_nAnd2::Bool->Bool->Bool
prop_nAnd2 a b = nAnd1 a b == False ||
                 nAnd1 a b == True


prop_nAnd3::Bool->Bool->Bool
prop_nAnd3 a b = nAnd2 a b == not(True) &&
                 nAnd2 a b == True

prop_nAnd4::Bool->Bool->Bool
prop_nAnd4 a b = nAnd2 a b == not(True) ||
                 nAnd2 a b == True

-- ================================================

{--
3.8) São dados 3 números inteiros, m,n e p. Se m for igual a n e n diferente de p, o resultado da função será True. Se m for diferente de n e n igual a p, o resultado da função será True. Se m for igual a n e n igual a p, o resultado da função será False. Se m for diferente de n e n diferente de p, o resultado será True.
--}
-- =================================================

-- 3.9)
threeDifferent:: Integer->Integer->Integer->Bool
threeDiffrent a b c
   | a /= b && a/= c && b/= c = True
   | otherwise = False
-- O resultado para threeDifferent 3 4 3 é: False
-- =================================================

-- 3.10)

fourDifferent1::Integer->Integer->Integer->Integer->Bool
fourDifferent1 a b c d
   | a == b && b == c && c == d = True
   | otherwise = False

fourDifferent2::Integer->Integer->Integer->Integer->Bool
fourDifferent2 a b c d = threeEqual (threeEqual a b c) d
   
threeEqual::Integer->Integer->Integer->Bool
threeEqual a b c (a==b) && (b==c)

-- ================================================

-- 3.11)
{--
threeEqual::Integer->Integer->Integer->Integer
threeEqual (2+3) 5 (11 'div' 2)
Resultado: True

mystery (2+4) 5 (11 'div' 2)  -- Não tem função definida!

threeEqual::Integer->Integer->Integer->Bool
threeEqual (2+4) 5 (11 'div' 2)
Resultado: False

fourEqual::Integer->Integer->Integer->Bool
fourEqual (2+4) 5 (11 'div' 2) (21 'mod' 11)
Resultado: False
--}
-- ================================================

-- 3.12)
prop_threeEqual1::Integer->Integer->Integer->Bool
prop_threeEqual1 a b c = threeEqual a b c == True &&
                         threeEqual a b c == False

prop_threeEqual2::Integer->Integer->Integer->Bool
prop_threeEqual2 a b c = threeEqual a b c == True ||
                         threeEqual a b c == False


prop_fourEqual1::Integer->Integer->Integer->Integer->Bool
prop_fourEqual1 a b c = threeEqual a b c d == True &&
                        threeEqual a b c d == False

prop_fourEqual1::Integer->Integer->Integer->Integer->Bool
prop_fourEqual1 a b c = threeEqual a b c d == True &&
                        threeEqual a b c d == False
-- =================================================

{-- 3.13)
max (3-2) (3*8)
Resultado: 24

maxThree (4+5) (2*6) (10 'div' 7)
Resultado: 12
--}
-- =================================================

-- 3.14)
min::Int->Int->Int
min a b = if a<=b then a else b

minThree::Int->Int->Int->Int
minThree a b c 
  | a<=b && b<=c = a
  | b<=c         = b
  | otherwise    = c
--=================================================

-- 3.15)
prop_maxThree1::Integer->Integer->Integer->Bool
prop_maxThree1 a b c = maxThree a b c >= a &&
                       maxThree a b c >= b &&
                       maxThree a b c >= c

prop_maxThree2::Integer->Integer->Integer->Bool
prop_maxThree2 a b c = maxThree a b c == a ||
                       maxThree a b c == b ||
                       maxThree a b c == c

prop_min1::Integer->Integer->Bool
prop_min1 a b = min a b <= a &&
                min a b <= b

prop_min2::Integer->Integer->Bool
prop_min2 a b = min a b == a ||
                min a b == b                      

prop_minThree1::Integer->Integer->Integer->Bool
prop_minThree1 a b c = minThree a b c <= a &&
                       minThree a b c <= b &&
                       minThree a b c <= c

prop_minThree2::Integer->Integer->Integer->Bool
prop_minThree2 a b c = minThree a b c == a ||
                       minThree a b c == b ||
                       minThree a b c == c
-- ==================================================

-- 3.16)
smallTOcaptial::Char->Char
smallTOcapital x  
    | x = toEnum(fromEnum x - fromEnum 'a' + fromEnum 'A')
    | otherwise = x
-- ==================================================

-- 3.17)
charTOnum::Char->Int
charTOnum y
  | fromEnum y = y
  | otherwise = y
-- ==================================================

-- 3.18)
-- onThreeLines::String->String->String->String

onThreeLines = do
putStrLn "Digite a primeira linha:"
a <- getLine
putStrLn "Digite a segunda linha:"
b <- getLine
putStrLn "Digite a terceira linha:"
c <- getLine
putStrLn ("\n"++a++"\n"++b++"\n"++c++"\n")
-- ===================================================

-- 3.19)
romanDigit :: Char -> String
romanDigit '0' = ""
romanDigit '1' = "I"
romanDigit '2' = "II"
romanDigit '3' = "III"
romanDigit '4' = "IV"
romanDigit '5' = "V"
romanDigit '6' = "VI"
romanDigit '7' = "VII"
romanDigit '8' = "VIII"
romanDigit '9' = "IX"
-- ==================================================

-- 3.10)
averageThree:: Integer->Integer->Integer->Float
averageThree x y z = (x+y+z)/3

howManyAboveAverage::Integer->Integer->Integer->Integer
howManyAboveAverage a b c  
  | a >= averageThree a b c = a
  | b >= averageThree a b c = b
  | otherwise = c
-- ===================================================

prop_avereageThree:: Integer->Integer->Integer->Bool
prop_averageThree x y z = averageThree x y z >= (x+y+z)/3 ||
                          averageThree x y z <= (x+y+z)/3

prop_howManyAboveAverage1::Integer->Integer->Integer->Bool
prop_howManyAboveAverage1 a b c = howManyAboveAverage a b c >= a &&
                                  howManyAboveAverage a b c >= b &&   
                                  howManyAboveAverage a b c >= c

prop_howManyAboveAverage2::Integer->Integer->Integer->Bool
prop_howManyAboveAverage2 a b c = howManyAboveAverage a b c >= a ||
                                  howManyAboveAverage a b c >= b ||  
                                  howManyAboveAverage a b c >= c
-- ====================================================

-- 3.22)
numberNDroots:: Float->Float->Float->Integer
numberNDroots a b c
  | b^2 > (4*a*c) = 2
  | b^2 == (4*a*c) = 1
  | otherwise = 0
-- ====================================================
-- 3.23)
numberRoots:: Float->Float->Float->Integer
numberRoots a b c
  | b^2 > (4*a*c) = 3
  | b^2 == (4*a*c) = 1
  | otherwise = 0
-- ====================================================

-- 3.24)
smallerRoots:: Float->Float->Float->Float
smallerRoots a b c
  | bˆ2 > (4*a*c) = ((-b)-(raizDeDelta a b c))/(2*a)
  | otherwise = 0
  

largerRoots:: Float->Float->Float->Float
largerRoots a b c
  | bˆ2 > (4*a*c) = ((-b)+(raizDeDelta a b c))/(2*a)
  | otherwise = 0
  
raizDeDelta::Float->Float->Float->Float
raizDeDelta a b c = ((b^2)-(4*a*c))**(1/2)
-- ====================================================

-- 3.25)
prop_smallerRoots1:: Float->Float->Float->Bool
prop_smallerRoots1 a b c = smallerRoots a b c >= ((-b)-(raizDeDelta a b c))/(2*a) &&
                          smallerRoots a b c >= 0  

prop_smallerRoots2:: Float->Float->Float->Bool
prop_smallerRoots2a b c = smallerRoots a b c <= ((-b)-(raizDeDelta a b c))/(2*a) ||
                          smallerRoots a b c <= 0  

prop_largerRoots1:: Float->Float->Float->Bool
prop_largerRoots1 a b c = smallerRoots a b c >= ((-b)+(raizDeDelta a b c))/(2*a) &&
                          smallerRoots a b c >= 0  

prop_largerRoots2:: Float->Float->Float->Bool
prop_largerRoots2 a b c = smallerRoots a b c <= ((-b)+(raizDeDelta a b c))/(2*a) ||
                          smallerRoots a b c <= 0  
-- ====================================================

{-- 3.27)
Se removermos o espaço antes em frente de "peculiar" a função deixa de ser local (pertencer a "funny" para ser global, podendo ser usada por qualquer outra função.
--}

-- 4.1)
maxFour1::Integer->Integer->Integer->Integer->Integer
maxFour1 a b c d
 | a>=b && a>=c && a>=d = a
 | b>=c && b>=d = b
 | c>=d = c
 | otherwise = d

maxFour2::Integer->Integer->Integer->Integer->Integer
maxFour2 a b c d 
 | a >= maxThree b c d = a
 | b >= maxThree a c d = b
 | c >= maxThree a b d = c
 | otherwise = d
 where
     maxThree::Integer->Integer->Integer->Integer
     maxThree x y z
        | a>=b && a>=c = a
        | b>=c = b
        | otherwise = c


maxFour3::Integer->Integer->Integer->Integer->Integer
maxFour3 a b c d
  | a >= maxThree b c d = a
 | b >= maxThree a c d = b
 | c >= maxThree a b d = c
 | otherwise = d
 where
     maxThree::Integer->Integer->Integer->Integer
     maxThree x y z
       | a >= max a b = a
       | b >= max c d = b
       | otherwise = c
       where
           max::Integer->Integer->Integer
           max y w
             | y>=w = y
             | otherwise = w
-- =======================================================

-- 4.2)
weakAscendingOrder::Integer->Integer->Integer->Bool
weakAscendingOrder a b c
  | a>=b && b>=c = True
  | otherwise = False
- ========================================================

-- 4,3)
howManyEqual::Integer->Integer->Integer->Integer
howManyEqual z y z
  | x==y && y==z = 3
  | (x==y && y/=z)||(x/=y && y==z) = 2
  | otherwise = 0
-- =======================================================

           