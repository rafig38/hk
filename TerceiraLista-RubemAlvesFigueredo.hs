import Data.Char
{--
Todos os exercícios dos slides "aula08 RecursoPrimitiva" e "aula10 RecursaoGeral".
Adicionalmente, os exercícios do livro do 7.8 a 7.26, exceto os exercícios 7.11, 7.15, 7.21, 7.22 e 7.23.
14 questoes do livro +  9 da aula8 + 16 da aula10 = 39
--}
-- EXERCÍCIOS DA AULA 8
-- 8.1
multDois::Float->Float->Float
multDois a b
  | (a ==0) || (b == 0) = 0
  | otherwise = a + multDois a (b-1)
-- =====================================
-- 8.2
-- Funcao que receba n e devola 2ˆn
potDois::Int->Int
potDois n
  | n == 0 = 1
  | otherwise = 2*potDois(n-1)
-- =====================================
-- 8.3
-- Recebe m e n devolve mˆn
potMN::Int->Int->Int
potMN m n
  | n == 0 && m == 0 = 0
  | n == 0 = 1
  | m == 0 = 0
  | otherwise = m*potMN m (n-1)
-- =====================================
-- 8.4
-- Escreva uma função que dado n, calcule: 0! + 1! + 2! + ... + n!
somaFatorial::Int->Int
somaFatorial n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n + 1 + somaFatorial (n-1)
-- =====================================
-- 8.5
-- Escreva uma função que calcule 2ˆ0 + 2ˆ1 + 2ˆ2 + ... + 2ˆn
somaPot::Int->Int
somaPot n
  | n == 0 = 1
  | otherwise = potDois n + somaPot (n-1)
-- =====================================
-- 8.6 - Não entendi o que, exatamente, se pede.
-- ====================================
{-- 8.7
Dada um função f de Int em Int, defina por recursão primitiva
uma função algumF0 que aceite um natural n e devolva o booleano
True se e somente se um ou mais valores de f 0, f 1, ..., f n é zero.
Teste com diferentes definições de f.
--}
algumF0 :: Int -> Bool
algumF0 n
  | f n == 0 = True
  | otherwise = AlgumF0 n-1

f :: Int -> Int
f m
| m == 0 = 8
| m == 1 = 44
| m == 2 = 17
| otherwise = 0

-- ====================================
-- 8.8
{--
Dada um função f de Int em Bool, defina por recursão primitiva
uma função algumFentre que aceite um natural n e devolva o booleano
True se e somente se f i é True para algum i entre 0 e n.
Teste com diferentes definições de f.
--}

algumFentre ::Int -> Bool
algumFentre 0 = True
algumFentre n
  | f n == True = True
  | otherwise = algumFentre (n-1)
       where f i = | i >= 0 && i<= n = True         
	               | otherwise = False
	
-- =====================================
-- 8.9
{--
Defina por recursão primitiva uma função que calcule a raiz
quadrada inteira de n (o maior natural cujo quadrado é menor ou
igual a n)
--}
raizQI :: Int -> Int
raizQI 0 = 0
raizQI n
  | (r+1)^2 == n = r + 1
  | otherwise = r
  where
    r = raizQI ( n-1 )
-- =====================================
-- 8.10
{--
Usando recursão primitiva sobre listas (não pode
usar compreensões), defina funções para
1– O produto dos elementos de uma lista de inteiros
2– Filtrar (eliminar) os números pares, ou seja, ficar somente
com os ímpares
3– Verificar se um string é formado somente por caracteres
alfanuméricos (letras e numerais). Use a função
isAlphaNum :: Char -> Bool
da biblioteca Data.Char
4– Eliminar a primeira ocorrência de um dado elemento, se
ele ocorrer, senão retornar a lista original
5– Eliminar todas as ocorrências de um dado elemento
6– Inverter um string
--}
-- 1-
prodInt :: [Int] -> Int
prodInt [] = 0
prodInt (x:xs) = x*prodInt(xs)
-- =====
-- 2-
filtrarPares :: [Int]->[Int]
filtrarPares [] = []
filtrarPares (x:xs) = acrescImpares (x:xs)
                    where acrescImpares (x:xs) = (x `mod` 2) /= 0 = x:filtrarPares xs
-- =====
-- 3-
verAlfaNum :: String -> Bool
verAlfaNum [] = False
verAlfaNum (s:st)
  | isAlphaNum s == elem s (s:st) = True
  | otherwise = verAlfaNum st
  
 -- =====
 -- 4-
eliminar :: Int ->[Int]->[Int]
eliminar _ [] = []
eliminar n (m:ms)
    | n == m = eliminar n ms
	| otherwise = (m:ms)
-- =====
-- 5-
eliminarT :: Int -> [Int] -> [Int]
eliminarT _ [] = []
eliminar n (m:ms)
  | n == m = elimnarT m ms
  | otherwise = eliminar n ms
-- =====
-- 6-
inverte :: String -> String
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]
-- ===============================================
{-- 8.11
A função or aplica o operador ou lógico || a todos os elementos
de uma lista. Por exemplo:
or [False, True, False]
↝  False || True || False ↝  … ↝  True
Dê uma definição recursiva para a função or.
--}
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs
-- ================================================
-- EXERCÍCIOS DA AULA 10
-- 10.1
{-- Definir propriedades da função maior e testar com
quickCheck --}
prop_maior :: [Int] -> Bool
prop_maior [] = False
prop_maior (x:xs)
  | max x (maior xs) == maximum (x:xs) = True
  | otherwise = False
-- ================================================
-- 10.2
{-- Definir uma função menor para calcular o menor de uma
lista de inteiros --}
menor :: [Int] -> Int
menor [x] = x
menor (x:xs) = min x (menor xs)
-- =================================================
-- 10.3
{-- Definir propriedades da função menor e testar com
quickCheck --}
prop_menor :: [Int] -> Bool
prop_menor [] = False
prop_menor (x:xs) 
  | min x (menor xs) == minimum (x:xs) = True
  | otherwise = False
-- =================================================
-- 10.4
{--
Definir a funcao:
filtraPosicoesPares :: [Int] -> [Int]
que retorna todos os elementos da lista de entrada que
estao em posicoes impares
--}
eliminarPosicoesPares :: [t] -> [t]
eliminarPosicoesPares [] = []
eliminarPosicoesPares [x] = []
eliminarPosicoesPares (x1:x2:xs) = x2:eliminarPosicoesPares(xs) 
-- =================================================
-- 10.5
{-- 
Definir a funcao:
filtraPosicoesImpares :: [Int] -> [Int]
que retorna todos os elementos da lista de entrada que
estao em posicoes pares
--}
eliminarPosicoesImpares :: [t] -> [t]
eliminarPosicoesImpares [] = []
eliminarPosicoesImpares [x] = [x]
eliminarPosicoesImpares (x1:x2:xs) = x1:eliminarPosicoesImpares(xs)

-- **************Do Livro*****************
--7.8
elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum a (x:xs)
    | a == x    = 1 + elemNum a xs
    | otherwise = elemNum a xs

testElemNum = TestList
    [ TestCase (assertEqual "" 0 (elemNum 5 []))
    , TestCase (assertEqual "" 0 (elemNum 5 [1,2,3]))
    ]
-- ================================================
-- do Livro: 
--7.9
unique :: [Integer] -> [Integer]
unique ls = [x | x <- ls, elemNum x ls == 1]

unique' :: [Integer] -> [Integer]
unique' [] = []
unique' (x : xs)
    | elemNum x (x:xs) == 1 = x : unique' xs
    | otherwise = unique' (deleteAll x xs)
  where
    deleteAll :: Integer -> [Integer] -> [Integer]
    deleteAll target ls = [_x | _x <- ls, _x /= target]
-- ================================================
--7.12
myMax :: [Integer] -> Integer
myMax [] = error "Lista Vazia"
myMax (x:xs) = go x xs
  where
    go m [] = m
    go m (y:ys)
        | m > y    = go m ys
        |otherwise =  go y ys

myMin :: [Integer] -> Integer
myMin [] = error "Lista Vazia"
myMin (x:xs) = go x xs
  where
    go m [] = m
    go m (y:ys)
        | m < y    = go m ys
        |otherwise =  go y ys

iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
   | x <= y = remove (x:(y:ys))
   |otherwise = y : ins x ys
-- ============================================
--7.14
isSorted :: [Integer] -> Bool
isSorted (x:xs) = head (iSort(x:xs)) == myMin (x:xs)
-- ============================================
--7.16
remove :: [Integer] -> [Integer]
remove [x] = [x]
remove [ ] = [ ]
remove (x:xs)
    | (elenNum x xs < 1) = x : remove xs -- Uso de elenNum. Não ter elementos repetidos
    | otherwise  = remove (xs)
-- ===========================================
--7.17
qSort :: [Int] -> [Int]

qSort [] = []
qSort (x:xs)
    = qSort [ y | y<-xs, y > x ] ++ [x] ++ qSort [ y | y<-xs, y < x ]
-- ==========================================
--7.18
sublist :: Eq a => [a] -> [a] -> Bool

sublist [] _ = True
sublist _ [] = False
sublist (x:xs) (y:ys)
    | x == y && sublist xs ys = True
    | sublist (x:xs) ys = True
    | otherwise = False

subs :: Eq a => [a] -> [a] -> Bool

subs [] _ = True
subs _ [] = False
subs (x:xs) (y:ys)
    | x == y && xs == [] = True
    | ys == [] && xs /= [] = False
    | x == y && x1 == y1 && subs x1s y1s = True
    | subs (x:xs) ys = True
    | otherwise = False
    where
    (x1:x1s) = xs
    (y1:y1s) = ys
-- ============================================
--7.20
mytake :: Int -> [a] -> [a]
mytake 0 _  = []
mytake _ [] = []
mytake n (a:as) = a: mytake (n-1) as

mydrop :: Int -> [a] -> [a]
mydrop _ []  = []
mydrop n (a:as)
    |n > 0  = mydrop (n-1) as
    |otherwise = (a:as)

mysplitAt :: Int -> [a] -> ([a],[a])
mysplitAt _ [] = ([],[])
mysplitAt n (a:as)
    | n > 0 = (mytake n (a:as), mydrop n (a:as))
    |otherwise = ([],(a:as))

-- usando quickCheck
prop_mydrop ::  Int -> [Int] -> Bool
prop_mydrop n ls
    |(mydrop n ls) == (drop n ls) = True
    |otherwise = False

prop_mysplitAt ::  Int -> [Int] -> Bool
prop_mysplitAt n ls
    |(mysplitAt n ls) == (splitAt n ls) = True
    |otherwise = False
-- ===============================================

--7.25
my_sublist :: Eq a => [a] -> [a] -> Bool
my_sublist [] [] = True
my_sublist _ [] = False
my_sublist [] _ = True
my_sublist (x:xs) (y:ys)
    |(x == y)  = my_sublist xs ys
    |otherwise = my_sublist (x:xs) ys

my_subsequence :: Eq a => [a]->[a]->Bool
my_subsequence [] [] = True
my_subsequence [] _  = True
my_subsequence _ []  = False
my_subsequence (x:xs) (y:ys)
    |(x == y) = my_subsequence xs ys
    |(x /= y) = my_subsequence (x:xs) ys
    |otherwise = my_subsequence (y:ys) xs
-- ================================================

--7.26
prop_sublist :: [Char] -> [Char] -> Bool
prop_sublist x y = (my_sublist x y)

prop_subsequence :: [Char] -> [Char] -> Bool
prop_subsequence x y = (my_subsequence x y)
-- ================================================
  
  
  