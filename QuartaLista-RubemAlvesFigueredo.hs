{- Dos slides "aula13-14 Gloss" os exercícios tapete de Sierpinsky e curva de Koch
 Todos os exercícios dos slides "aula15 polimorfismoETypeClasses"
-}

--aula13-14 gloss
-- TAPETE --
module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "fractal quadrado" (500, 500) (-20, -20)

background :: Color
background = white

tapete:: Int -> Point -> Float -> Picture
tapete 0 _ _ = blank
tapete n (x, y) sz = pictures [ polygon [(x+sz, y+sz), (x+sz, y+2*sz), (x+2*sz, y+2*sz), (x+2*sz, y+sz)]
                  ,tapete (n-1) (x, y) (sz/3)
                  ,tapete (n-1) (x+sz, y) (sz/3)
                  ,tapete (n-1) (x+2*sz, y) (sz/3)
                  ,tapete (n-1) (x, y+sz) (sz/3)
                  ,tapete (n-1) (x+2*sz, y+sz) (sz/3)  
                  ,tapete (n-1) (x, y+2*sz) (sz/3) 
                  ,tapete (n-1) (x+sz, y+2*sz) (sz/3) 
                  ,tapete (n-1) (x+2*sz, y+2*sz) (sz/3) ]
                            
main :: IO ()
main = display window background (tapete 4 (-150, -150) 200)

-- Curva de Koch --
odule Main(main) where
import Graphics.Gloss

window :: Display
window = InWindow "Curva de Koch" (500, 500) (-20, -20)

ckoch:: Int -> Point -> Point -> Picture
ckoch 0 p q = line [p, q]
ckoch n p q = pictures [ ckoch (n-1) p p1
                         ,ckoch (n-1) p1 p2
                         ,ckoch (n-1) p2 p3
                         ,ckoch (n-1) p3 q ]                         
               where v = mulSV (1/3) (q V.-p)
                   p1 = p V.+v
                   p2 = p1 V.+rotateV (45) v
                   p3 = p2 V.+rotateV (-45) v
                           

main :: IO ()
main = display window white (ckoch 4 360 360)

--aula15 polimorfismoETypeClasses

{- 1º Considere a seguinte função:

shift ((x,y), z) = (x, (y, z))
Qual é o seu tipo mais geral?

R- shift :: ((a,b), c) -> (a, (b, c))
-}
shift :: ((a,b),c) -> (a, (b,c))
shift ((x, y), z) = (x, (y, z))


{-2º Considere a seguinte função:

zip' [] ps = []
zip' ps [] = []
zip' (p:ps) (q:qs) = (p, q) : zip' ps qs

O que ela calcula? Qual é seu tipo mais geral?

R - A função zip' cria uma lista de tuplas com pares de elementos correspondentes
    exmplo:
    zip' [1..5] "rubem"
    >> [(1,'r'),(2,'u'),(3,'b'),(4,'e'),(5,'m')]

   tipo mais geral [a] -> [b] -> [(a,b)]

-}
zip' :: [a] -> [b] -> [(a,b)]
zip' [] ps = []
zip' ps [] = []
zip' (p:ps) (q:qs) = (p, q) : zip' ps qs

{-3º
Defina uma função numEqual que pegue uma lista xs de items e
um item x e retorne o número de vezes que x ocorre dentro de xs.
Qual é o tipo da sua função? Como poderia usar numEqual para
definir elem?

R - A função é do tipo Eq a => a -> [a] -> Int
    elem iria verificar a contagem de numEqual, digo,
    se for maior ou igual a 1 será True, caso contrário retorna False.


-}
numEqual :: Eq a => a -> [a] -> Int -- A função permite qualquer tipo, desde que os tipos permitam a comparação (==).
numEqual _ [] = 0
numEqual x (y:ys)
    | x == y = 1 + numEqual x ys
    | otherwise =  numEqual x ys

elem' :: Eq a => a -> [a] -> Bool
elem' x xs
    | (numEqual x xs) >= 1 = True -- Para ser True o elemento deve aparecer PELO MENOS UMA VEZ.
    | otherwise            = False

{-4º
Defina a função oneLookupFirst que pega uma lista de pares e
um item. Digamos que o tipo dos pares é (a, b), e que o tipo do
item é a. A função retorna a segunda componente do primeiro par
cuja primeira componente é igual ao item. Qual é o tipo mais geral da
função?
R -
  Eq a => a -> [(a,b)] -> b

Defina a função oneLookupSecond que retorna a primeira
componente do primeiro par cuja segunda componente é igual ao
item.
Qual é o tipo mais geral da função?
R -
  Eq b => b -> [(a,b)] -> a
-}
oneLookupFirst :: Eq a => a -> [(a,b)] -> b
oneLookupFirst _ [] = error "Elemento não encontrado, ou lista vazia!"
oneLookupFirst x (y:ys)
   | x == fst(y) = snd (y)
   | otherwise = oneLookupFirst x (ys)

oneLookupSecond :: Eq b => b -> [(a,b)] -> a
oneLookupSecond _ [] = error "Elemento não encontrado, ou lista vazia!"
oneLookupSecond x (y:ys)
   | x == snd(y) = fst (y)
   | otherwise = oneLookupSecond x (ys)

{-5º Considere a seguinte função
misterio y x = [ show z | z <- x, elem z y ]
Qual é eu seu tipo mais geral?
R -
    misterio recebe duas listas. E retorna uma terceira lista com seus elementos convertidos
    em string. A nova lista (z) irá receber apenas valores que está contido nas duas listas.

    tipo geral é misterio :: (Eq a, Show a) => [a] -> [a] -> [String]

-}
misterio :: (Eq a, Show a) => [a] -> [a] -> [String]
misterio y x = [ show z | z <- x, elem z y ]