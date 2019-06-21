{--
Todos os exercícios dos slides "aula08 RecursoPrimitiva" e "aula10 RecursaoGeral".
Adicionalmente, os exercícios do livro do 7.8 a 7.26, exceto os exercícios 7.11, 7.15, 7.21, 7.22 e 7.23.
14 questoes do livro +  9 da aula8 + 16 da aula10 = 39
--}
-- Exercicio 8.1
multDois::Float->Float->Float
multDois a b
  | (a ==0) || (b == 0) = 0
  | otherwise = a + multDois a (b-1)
-- =====================================
-- Exercicio 8.2
-- Funcao que receba n e devola 2ˆn
potDois::Int->Int
potDois n
  | n == 0 = 1
  | otherwise = 2*potDois(n-1)
-- =====================================
-- Exercicio 8.3
-- Recebe m e n devolve mˆn
potMN::Int->Int->Int
potMN m n
  | n == 0 && m == 0 = 0
  | n == 0 = 1
  | m == 0 = 0
  | otherwise = m*potMN m (n-1)
-- =====================================
-- Exercicio 8.4
-- Escreva uma função que dado n, calcule: 0! + 1! + 2! + ... + n!
somaFatorial::Int->Int
somaFatorial n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n + 1 + somaFatorial (n-1)
-- =====================================
-- Exercicio 8.5
-- Escreva uma função que calcule 2ˆ0 + 2ˆ1 + 2ˆ2 + ... + 2ˆn
somaPot::Int->Int
somaPot n
  | n == 0 = 1
  | otherwise = potDois n + somaPot (n-1)
-- =====================================
-- Exercicio 8.6
{--
f :: Int -> Int
f m
  | m == 0 = 8
  | m == 1 = 44
  | m == 2 = 17
  | otherwise = 0
 --}
 {--
 f :: Int -> Int
 f m
   | m == 0 = 8
   | m == 1 = 44
   | m == 2 = 17
   | m > maiorF m = m
   | otherwise = 0

maiorF :: Int -> Int
maiorF x
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 2
  | otherwise =  --}
-- ====================================
{-- Exercicio 8.7
Dada um função f de Int em Int, defina por recursão primitiva
uma função algumF0 que aceite um natural n e devolva o booleano
True se e somente se um ou mais valores de f 0, f 1, ..., f n é zero.
Teste com diferentes definições de f.
--}
f :: Int -> Int
f n
  | n == 0 = 0
  | algumF0 n /= True = n
  | otherwise = n

algumF0 :: Int -> Bool
algumF0 n
  | n == 0 = True
  | n /= 0 = algumF0 n
  | otherwise = False
-- ====================================
{--
Dada um função f de Int em Bool, defina por recursão primitiva
uma função algumFentre que aceite um natural n e devolva o booleano
True se e somente se f i é True para algum i entre 0 e n.
Teste com diferentes definições de f.
--}

algumFentre ::Int -> Bool
algumFentre i
  | i >= 0 && i<= n = True
  | otherwise = False
