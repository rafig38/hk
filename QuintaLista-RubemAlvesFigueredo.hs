{-
Todos os exercícios nos slides "aula15-16-17 FuncoesComoArgumentos.pdf" e no arquivo "aula18-19 AltaOrdem.hs"
-}
import Data.Char
import Data.List


-- Qual o tipo mais geral de fiter?
-- Resp.: filter :: (a -> Bool)->[a] -> [a]

{-2ºResolva os problemas 1 e 4 da
primeira prova sem usar nem
compreensões e nem recursão
-}
-- Q1:
senAprox :: Int -> Float -> Float
senAprox n x = sum (map f [0..n])
    where 
        f i = (((-1)^i)*(x^((2*i)+1)))/fromIntegral(fat((2*i)+1))
        fat n = product[1..n]
-- 4)        
separaDigitos :: String -> (String, String)
separaDigitos [] = ([],[])
separaDigitos xs = (filter isDigit xs, filter notDigit xs)

notDigit :: Char -> Bool
notDigit c = not (isDigit c)

-- Defina length usando map e sum
length' :: Num a => [a] -> Int
length' [] = 0
length' xs = sum (map (\x->1) xs)
        
-- Qual é o efeito de: 
-- map addOne (map addOne ns) ?
--   |                |-> retorna uma lista com cada
--   |                    elemento adicionado uma unidade.
--   |-> retorna uma lista com os elementos da 1a lista
--       retornada acima, acrescida novamente de uma unidade.
--       conclui-se então que, pode-se usar o resultado de uma 
--       função como argumento de outra, in line.
--       Semelhante a função twice aplicando addOne sobre a lista ns.
-- Defina funções que tomem uma lista, ns, e: 
{-
 .retorne a lista consistindo dos quadrados dos inteiros em
  ns: 
-}
quadInt :: Integral a => [a] -> [a]
quadInt [] = []
quadInt ns = map (\x->x*x) ns
      
-- .retorne a soma dos quadrados dos itens em ns:
somaQuad :: Integral a => [a] -> a
somaQuad ns = foldr (+) 0 (map (\x -> x^2) ns)

-- .verifique se todos os itens da lista são maiores que zero:
maiorQZero :: [Int] -> Bool
maiorQZero ns = and( map g ns )
            where
                g x = if x > 0 then True else False

-- Defina funções para:
-- 1. calcular o menor valor de uma função f aplicada de 0 até n
menorDe :: Int -> Int
menorDe 0 = 0
menorDe n = minimum  $ map (\x->x) [1..n]
       
-- 2. verificar se os valores de f aplicados de 0 até n são todos
--    iguais:
-- Com recursão: 
seIguais ::Eq a => [a] -> [a]
seIguais (x:y:xs)
    | x==y = x:y:seIguais (y:xs)
    | otherwise = seIguais (y:xs)
           
-- Alta ordem:
seIguais':: Int ->(Int -> Int)-> Bool 
seIguais' n f = and $ map (\x -> x==(f 0))(map f [0..n])

-- 3. verificar se todos os valores de f aplicados de 0 até n são
--    maiores que 0
seMaiorQzero:: Int -> (Int -> Int)-> Bool 
seMaiorQzero n f = and $ map (\x -> x>(f 0))(map f [0..n])

-- 4. verificar se os valores de f aplicados de 0 até n estão em
--    ordem crescente
seCrescente::Int -> Bool 
seCrescente n = and (map g ls)
              where
                 ls = map f [0..n]
                 f x = x
                 g x = if (succ x) >= x then True else False

{-
p ["Penso", "logo", "existo"] [',', ' ', '.'] =
"Penso,logo existo."
-}
p :: [String] -> [Char] -> String
p xss ys = foldr (++) []  $ map (\(x,y)->x++[y]) dupla 
       where 
          dupla = zip xss ys

{-
Estabeleça o tipo e defina uma função twice que aceita
uma função e um valor e aplica esta função duas vezes.
Por exemplo, a função twice aplicada as entradas
double e a 7 produzirá 28 como resultado.
● Defina o tipo e defina a função iter tal que
iter n f x = f (f (f … (f x)…))
onde f ocorre n vezes no lado direito da equação. Por
exemplo, deveríamos ter que
iter 3 f x = f (f (f x)))
● Usando iter e double defina uma função a qual para a
entrada n retorna 2^n
-}

iter :: Int -> (a -> a) -> (a -> a)
iter n f = foldr (.) id fs
    where
    	fs = map (\_->f) [1..n]    

twice :: (Int -> Int) -> (Int -> Int)
twice f = iter1 2 f

{-Calcule a soma dos quadrados dos números naturais 1 até n usando
map e foldr
● Defina uma função que dê a soma dos quadrados dos inteiros positivos
de uma lista de inteiros -}
somaQ :: Integral a => a -> a
somaQ n = foldr (+) 0 (map (\x -> x^2) [1..n])

-- ● Usando foldr defina as funções unzip, last e init
unzip'::[(a,b)]->([a], [b])
unzip'  = foldr f ([],[]) 
      where
          f (x,y)(xs,ys) = (x:xs, y:ys)


last' :: [a] -> a
last' = foldr1 (\_ n -> n) 
    

init' :: [a]->[a]
init' xs = foldr1 (++) ns
    where
      ns = reverse (drop 1 (reverse (map (\x->[x]) xs)))

{-
O que calcula a seguinte função?:
  misterio xs = foldr (++) [] (map sing xs)
           where sing x = [x] 
Resp.: misterio cria uma string de Char, elementos da lista "xs"
-}
misterio::[Char] -> String
misterio xs = foldr (++) [] (map sing xs)
       where 
           sing x = [x] 

{-
Defina uma função:
filterFirst :: (a -> Bool) -> [a] -> [a]
tal que filterFirst p xs remova o primeiro elemento de xs que não
satisfaz a propriedade p.
-}
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p xs = dropWhile (\x->False) xs
             {-
Defina:
filterLast :: (a -> Bool) -> [a] -> [a]
que remove a última ocorrência de um elemento de uma lista que não
satisfaz a propriedade.
-}
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = takeWhile (\x->True) xs
{-
Defina a função switchMap que aplica de forma alternada duas
funções aos elementos de uma lista. Por exemplo
switchMap addOne addTen [1,2,3,4] ↝ [2,12,4,14]
-}
switchMap :: (Int -> Int) -> (Int -> Int) -> [Int] -> [Int]
switchMap f g xs = merge (map f (filter odd xs)) (map g (filter even xs))

{-
Defina funções
split :: [a] -> ([a], [a])
merge :: ([a], [a]) -> [a]
tal que split divide em duas listas, pegando alternadamente,
enquanto merge intercala as duas listas. Por exemplo
split [1,2,3,4,5] ↝ ([1,3,5], [2,4])
merge ([1,3,5], [2,4]) ↝ [1,2,3,4,5]
-}   
merge :: ([Int],[Int]) ->[Int]
merge ([],[]) = []
merge (xs,[]) = xs
merge ([],ys) = ys
merge xs ys = f ((map (\x->x) xs) (map (\y->y) ys))
        where
            f (x:xs)(y:ys) = x:y:(f xs ys) 

split :: [Int] -> ([Int],[Int]) 
split [] = ([],[])
split xs = ((map (\x->x) (filter odd xs)), (map (\x->x) (filter even xs)))

{-
Defina as funções takeWhile e dropWhile
Quais são seus tipos mais gerais?
-}
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f(x:xs)= if f x = True then x:(takeWhile' p xs ) else []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x = True then dropWhile' f xs else (x:xs)

-- Qual é o tipo mais geral de twice ?
-- twice :: (Int -> Int) -> (Int -> Int)

-- 11.3 do livro Haskell: the craft
composeList ::[(a->a)]->(a->a)
composeList [f,g] = foldr (.) id [f,g]
-- O tipo acima é genérico,mas de tipo homogêneo, visto que, composeList 
-- recebe uma lista de funções e uma lista é composta de elementos de mesmo tipo
-- id é o caso base e corresponde a função identidade.

-- Qual é o tipo do operador de aplicação $?
--     ($) :: (a->b)->a->b

-- Considerando que id é afunção identidade, explique qual é o
-- comportamento de cada expressão:
--   + id $ f -> f é uma função se comportando como argumento de id -> id(f)
--   + f $ id -> id é uma função se comportando como argumento f -> f(id) 
--   + id ($) -> id e a composição "$" -> id (...)

-- Defina a generalização de twice
-- iter :: Int -> (a -> a) -> (a -> a)
-- tal que
-- iter n f
-- é a composição de f com f, n vezes.
-- + Usando iter, defina a função
-- pot2 :: Int -> Int
-- tal que pot2 n = 2^n
iter1 :: Int -> (a -> a) -> (a -> a)
iter1 n f = foldr (.) id fs
    where
    	fs = map (\_-> f) [1..n]
    	

pot2 :: Int -> Int
pot2 n = (iter1 n dobro) 1
    where
    	dobro x = 2 * x

-- 11.7, 11.8, 11.9 e 11.10 do livro Haskell: the craft 2ed

{- Usando ranges, map e expressões lambda, defina replicate:
  replicate :: int -> a -> [a]
  tal que:
  replicate n x devolva uma lista formada por n x's.
-}
replicate :: int -> a -> [a]
replicate n x = map (\x -> x) $ [1..n]

-- Usando replicate e foldr, defina iter:              
iter' :: Int -> (a -> a) -> (a -> a)
iter' n f = foldr (.) id $ map (\_-> f) $ [1..n]


