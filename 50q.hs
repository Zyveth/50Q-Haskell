-- 1
-- Apresente uma definiçãao recursiva da funçãao (pré-definida)
-- enumFromTo :: Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites.
-- Por exemplo, enumFromTo 1 5 corresponde à lista [1,2,3,4,5]

myEnumFromTo :: Int -> Int  -> [Int]
myEnumFromTo a b | a > b = []
                 | otherwise = a : myEnumFromTo (a + 1) b

-- 2
-- Apresente uma definição recursiva da função (pré-definida)
-- enumFromThenTo :: Int -> Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois 
-- limites e espaçados de um valor constante.
-- Por exemplo, enumFromThenTo 1 3 10 corresponde à lista [1,3,5,7,9]

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c | a > c = []
                       | otherwise = a : myEnumFromThenTo (a + (b - a)) (b + (b - a)) c

-- 3
-- Apresente uma definição recursiva da função (pré-definida)
-- (++):: [a] -> [a] -> [a] que concatena duas listas.
-- Por exemplo, (++) [1,2,3] [10,20,30] corresponde à lista [1,2,3,10,20,30].

(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h : t) l = h : (+++) t l

-- 4
-- Apresente uma definição recursiva da função (pré-definida) 
-- (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento  da lista que se encontra nessa posição
-- (assume- se que o primeiro elemento se encontra na posição 0).
-- Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
-- Ignore os casos em que a função não se encontra definida (i.e., em que a posição fornecida não
-- corresponde a nenhuma posição válida da lista).

(!!!) :: [a] -> Int -> a
(!!!) [] _ = undefined
(!!!) (h : t) 0 = h
(!!!) (h : t) n = (!!!) t (n - 1)

-- 5
-- Apresente uma definição recursiva da função (pré-definida)
-- reverse :: [a] -> [a] que dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
-- Por exemplo, reverse [10,20,30] corresponde a [30,20,10].

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h : t) = myReverse t +++ [h]

-- 6
-- Apresente uma definição recursiva da função (pré-definida)
-- take :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros 
-- elementos de l.
-- A lista resultado só terá menos de que n elementos se a lista l tiver menos do que n elementos. Nesse caso a lista 
-- calculada é igual à lista fornecida.
-- Por exemplo, take 2 [10,20,30] corresponde a [10,20].

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (h : t) = h : myTake (n - 1) t

-- 7
-- Apresente uma definição recursiva da função (pré-definida)
-- drop :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros 
-- elementos de l.
-- Se a lista fornecida tiver n elementos ou menos, a lista resultante será vazia. 
-- Por exemplo, drop 2 [10,20,30] corresponde a [30].

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop n (h : t) = myDrop (n - 1) t

-- 8
-- Apresente uma definição recursiva da função (pré-definida)
-- zip :: [a] -> [b] -> [(a,b)] constrói uma lista de pares a partir de duas listas.
-- Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (h1 : t1) (h2 : t2) = (h1, h2) : myZip t1 t2

-- 9
-- Apresente uma definição recursiva da função (pré-definida)
-- elem :: Eq a => a -> [a] -> Bool que testa se um elemento ocorre numa lista.
-- Por exemplo, elem 20 [10,20,30] corresponde a True enquanto que elem 2 [10,20,30] corresponde a False.

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (h : t) | x == h = True
                 | otherwise = myElem x t

-- 10
-- Apresente uma definição recursiva da função (pré-definida)
-- replicate :: Int -> a -> [a] que dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais 
-- a x.
-- Por exemplo, replicate 3 10 corresponde a [10,10,10].

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- 11
-- Apresente uma definição recursiva da função (pré-definida)
-- intersperse :: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é
-- intercalado entre os elementos da lista fornecida.
-- Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x [h] = [h]
intersperse x (h : t) = h : x : intersperse x t

-- 12
-- Apresente uma definição recursiva da função (pré-definida)
-- group :: Eq a => [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista.
-- Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].

group :: Eq a => [a] -> [[a]]
group [] = []
group (h : t) = takeEquals h (h : t) : group (dropEquals h (h : t))

takeEquals :: Eq a => a -> [a] -> [a]
takeEquals _ [] = []
takeEquals x (h : t) | x == h = h : takeEquals x t
                     | otherwise = []

dropEquals :: Eq a => a -> [a] -> [a]
dropEquals _ [] = []
dropEquals x (h : t) | x == h = dropEquals x t
                     | otherwise = h : t

