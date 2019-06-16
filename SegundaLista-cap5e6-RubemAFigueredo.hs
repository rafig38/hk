-- Segunda lista obrigatória
-- 5.1a)
maxOccurs::Integer->Integer->(Integer, Integer)
maxOccurs a b
  | a >= b = (b, a)
  | otherwise = (a, b)
  
-- ====================================================================
-- 5.1b)
maxThreeOccurs:: Integer->Integer->Integer->(Integer, Integer, Integer)
maxThreeOccurs x y z  
  | y >= x && z >= y = (x, y, z)
  | x >= y && z >= x = (y, x, z)
  | z >= y && x >= z = (y, z, x)
  | otherwise = (z, y, x)
   
-- =====================================================================
-- 5.2)
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (u, v, w) = maxThreeOccurs u v w

-- ===================================================================== 
-- 5.3)
crossX :: (Int, Int) -> Bool
crossX (a, b) = if b == 0 then True else False
 
-- =====================================================================
{-- 5.4)

Para o 5.1a:
a b | 1 2 -> (1, 2)
a b | 3 1 -> (1, 3)

Para o 5.1b:
x y z | 3 1 2 -> (1, 2, 3)
x y z | 3 2 1 -> (1, 2, 3)
x y z | 2 3 1 -> (1, 2, 3)

Para o 5.2:
u v w | (3, 1, 2) -> (1,2,3)
u v w | (1, 3, 2) -> (1,2,3)
u v w | (2, 3, 1) -> (1,2,3)

Para o 5.3:
Ver se é ponto de cruzamento do eixo X 
(2, 1) -> Falso
(2, 0) -> Verdadeiro
--}
-- =====================================================================

-- 5.18)

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*x | x <- xs]
-- ======================================================================

-- 5.19)

capitalize :: String -> String
capitalize st = [capX x | x <- st]
   where capX x = toEnum(fromEnum x - fromEnum 'a' + fromEnum 'A')

   
capitalizeLetters :: String -> String
capitalizeLetters st = [capX x | x <- st, not(ehNumero x)]
   where capX x = toEnum(fromEnum x - fromEnum 'a' + fromEnum 'A')
         ehNumero x = '0'<=x && x<='9'  
-- ======================================================================

-- 5.20)
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]
 
isPrime :: Integer -> Bool
isPrime p = if [x | x <- [1 .. p], p `mod` (x) == 0] == [1, p] then True else False

-- ======================================================================

-- 5.21)

matches :: Integer ->[Integer] -> [Integer]
matches m ms  
  | elem m ms = [ n | n <- ms, n == m]
  | otherwise = []
  
v_elem :: Int -> [Int] -> Bool 
v_elem a [] = False
v_elem a (x:xs) | a == x || v_elem a xs = True
                | otherwise = False
 
--import Prelude hidding (v_elem)
 -- ====================================================================
 
 -- 5.22)
 
onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs
-- =====================================================================
 
-- 5.23)
{--
pushRight :: String -> String
pushRight str = x++str
  | lineLength >= 12 = d
  | otherwise = lineLength
  where d = (lineLength - (length str))
  comp = [x | " " <- [1..d]]
 --}

-- Make lineLength a pushRight's parameter:

pushRight :: String -> Int - > String
pushRight str linelength = x++str
  | lineLength >= 12 = d
  | otherwise = lineLength
  where d = (lineLength - (length str))
  comp = [x | " " <- [1..d]]
-- ======================================================================  

-- 5.27)
type Person = String

type Book = String


type exampleBase = [ (Person, Book) ]

books :: exampleBase -> Person -> [Book]

books [] p = []

books ((p1, lv):bd) p
  
  | p == p1 = lv: books bd p
  
  | otherwise = books e p

-- ======================================================================

-- 5.28)

type Person = String

type Book = String


type exampleBase = [ (Person, Book) ]

borrowers :: exampleBase -> Book -> [Person]

borrowers [] lv = []

borrowers ((p, lv1):bd) lv
  
  | lv1 == lv = p: borrowers bd lv
  
  | otherwise = borrowers bd lv


borrowed :: examplBase -> Book -> Bool

borrowed [] lv = False
borrowed ((p, lv1):bd) lv

  | lv1 == lv = True
  | otherwise = False


numBorrowed :: exampleBase -> Person -> Int

numBorrowed [] _ = 0

numBorrowed ((p1, lv):bd) p
  
  | p == p1 = 1 + numBorrowed  bd p
  
  | otherwise = numBorrowed  bd p




-- ======================================================================

-- 5.29)
type Person = String

type Book = String


type exampleBase = [ (Person, Book) ]
returnLoan :: exampleBase -> Person -> Book -> exampleBase
returnLoan [] p lv = []

returnLoan (p2, lv2):bd) p lv
  
  | p==p2 && lv==lv2 = (p,lv):returnoLoan bd p lv
  
  | otherwise = returnoLoan bd p lv
-- ======================================================================








