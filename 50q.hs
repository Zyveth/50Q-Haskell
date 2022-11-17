-- 1
-- Apresente uma definiçãao recursiva da funçãao (pré-definida)
-- enumFromTo :: Int -> Int -> [Int] que constrói a lista dos 
-- números inteiros compreendidos entre dois limites.
-- Por exemplo, enumFromTo 1 5 corresponde à lista [1,2,3,4,5]

myEnumFromTo :: Int -> Int  -> [Int]
myEnumFromTo a b | a > b = []
                 | otherwise = a : myEnumFromTo (a + 1) b

-- 2
-- Apresente uma definição recursiva da função (pré-definida)
-- enumFromThenTo :: Int -> Int -> Int -> [Int] que constrói
-- a lista dos números inteiros compreendidos entre dois limites
-- e espaçados de um valor constante.
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
-- (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento 
-- da lista que se encontra nessa posição (assume- se que o primeiro elemento se encontra na posição 0).
-- Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
-- Ignore os casos em que a função não se encontra definida (i.e., em que a posição fornecida não
-- corresponde a nenhuma posição válida da lista).

(!!!) :: [a] -> Int -> a
(!!!) [] _ = undefined
(!!!) (h : t) 0 = h
(!!!) (h : t) n = (!!!) t (n - 1)