-- 1
-- Apresente uma definição recursiva da função (pré-definida)
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

-- 13
-- Apresente uma definição recursiva da função (pré-definida)
-- concat :: [[a]] -> [a] que concatena as listas de uma lista.
-- Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h : t) = h +++ myConcat t

-- 14
-- Apresente uma definição recursiva da função (pré-definida)
-- inits:: [a] -> [[a]] que calcula a lista dos prefixos de uma lista.
-- Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].

inits :: [a] -> [[a]]
inits [] = [[]]
inits (h : t) = inits (take (length (h : t) - 1) (h : t)) +++ [h : t]

-- 15
-- Apresente uma definição recursiva da função (pré-definida)
-- tails:: [a] -> [[a]] que calcula a lista dos sufixos de uma lista.
-- Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].

tails :: [a] -> [[a]]
tails [] = [[]]
tails (h : t) = [h : t] +++ tails t

-- 16
-- Apresente uma definição recursiva da função (pré-definida)
-- isPrefixOf:: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra.
-- Porexemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
-- [10,30] [10,20,30] corresponde a False.

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h1 : t1) (h2 : t2) | h1 == h2 = isPrefixOf t1 t2
                               | otherwise = False

-- 17
-- Apresente uma definição recursiva da função (pré-definida)
-- isSuffixOf:: Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra.
-- Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
-- [10,30] [10,20,30] corresponde a False.

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l1 l2 = reverse l1 `isPrefixOf` reverse l2

-- 18
-- Apresente uma definição recursiva da função (pré-definida)
-- isSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem 
-- noutra pela mesma ordem relativa.
-- Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que 
-- isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h1 : t1) (h2 : t2) | h1 == h2 = isSubsequenceOf t1 t2
                                    | otherwise = isSubsequenceOf (h1 : t1) t2


-- 19
-- Apresente uma definição recursiva da função (pré-definida)
-- elemIndices :: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado elemento 
-- ocorre numa lista.
-- Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6].

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x (h : t) | x == h = 0 : map (1+) (elemIndices x t)
                      | otherwise = map (1+) (elemIndices x t)

-- 20
-- Apresente uma definição recursiva da função (pré-definida)
-- nub :: Eq a => [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem repetições.
-- Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].

nub :: Eq a => [a] -> [a]
nub [] = []
nub (h : t) = h : nub (removeEquals h t)

removeEquals :: Eq a => a -> [a] -> [a]
removeEquals _ [] = []
removeEquals x (h : t) | x == h = removeEquals x t
                       | otherwise = h : removeEquals x t


-- 21
-- Apresente uma definição recursiva da função (pré-definida)
-- delete :: Eq a => a -> [a] -> [a] que retorna a lista resultante de remover (a primeira ocorrência de) 
-- um dado elemento de uma lista.
-- Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir nenhuma ocorrência a 
-- função deverá retornar a lista recebida.

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h : t) | x == h = t
                 | otherwise = h : delete x t

-- 22
-- Apresente uma definição recursiva da função (pré-definida)
-- (\\):: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover (as primeiras ocorrências) dos 
-- elementos da segunda lista da primeira.
-- Por exemplo, (\\) [1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1].

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) l (h : t) = (\\) (delete h l) t

-- 23. Apresente uma definição recursiva da função (pré-definida)
-- union :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de acrescentar à primeira lista os elementos 
-- da segunda que não ocorrem na primeira.
-- Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5].

union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h : t) | myElem h l = union l t
                | otherwise = union (l +++ [h]) t

-- 24
-- Apresente uma definição recursiva da função (pré-definida)
-- intersect :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos 
-- que não pertencem à segunda.
-- Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h : t) l | myElem h l = h : intersect t l
                    | otherwise = intersect t l

-- 25
-- Apresente uma definição recursiva da função (pré-definida)
-- insert :: Ord a => a -> [a] -> [a]  que dado um elemento e uma lista ordenada retorna a lista resultante de inserir
-- ordenadamente esse elemento na lista.
-- Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40].

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h : t) | x < h = x : h : t
                 | otherwise  = h : insert x t

-- 26
-- Apresente uma definição recursiva da função (pré-definida)
-- unwords :: [String] -> String que junta todas as strings da lista numa só, separando-as por um espaço.
-- Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional".

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords (h : t) = h +++ " " +++ unwords t

-- 27
-- Apresente uma definição recursiva da função (pré-definida)
-- unlines :: [String] -> String que junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
-- Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".

myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (h : t) = h +++ "\n" +++ myUnlines t

-- 28
-- Apresente uma definição recursiva da função
-- pMaior :: Ord a => [a] -> Int que dada uma lista não vazia, retorna a posição onde se encontra o maior elemento 
-- da lista. As posições da lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista 
-- for o maior.

pMaior :: Ord a => [a] -> Int
pMaior [] = undefined
pMaior [x] = 0
pMaior l = index (maxArray l) l 

maxArray :: Ord a => [a] -> a
maxArray [] = undefined
maxArray [x] = x
maxArray (h : t) = if h > maxArray t then h else maxArray t

index :: Eq a => a -> [a] -> Int
index _ [] = undefined
index x (h : t) = if x == h then 0 else index x t + 1