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

-- 29
-- Apresente uma definição recursiva da função
-- temRepetidos :: Eq a => [a] -> Bool que testa se uma lista tem elementos repetidos.
-- Por exemplo, temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos [11,2,31,4] corresponde a 
-- False.

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h : t) | myElem h t = True
                     | otherwise = temRepetidos t

-- 30
-- Apresente uma definição recursiva da função
-- algarismos :: [Char] -> [Char] que determina a lista dos algarismos de uma dada lista de caracteres.
-- Por exemplo, algarismos "123xp5" corresponde a "1235".

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h : t) | fromEnum h >= 48 && fromEnum h <= 57 = h : algarismos t
                   | otherwise = algarismos t

-- 31
-- Apresente uma definição recursiva da função
-- posImpares :: [a] -> [a] que determina os elementos de uma lista que ocorrem em posições ímpares. Considere que 
-- o primeiro elemento da lista ocorre na posição 0 e por isso par.
-- Por exemplo, posImpares [10,11,7,5] corresponde a [11,5].

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h1 : h2 : t) = h2 : posImpares t

-- 32
-- Apresente uma definição recursiva da função
-- posPares :: [a] -> [a] que determina os elementos de uma lista que ocorrem em posições pares. Considere que 
-- o primeiro elemento da lista ocorre na posição 0 e por isso par.
-- Por exemplo, posPares [10,11,7,5] corresponde a [10,7].

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (h1 : h2 : t) = h1 : posPares t

-- 33
-- Apresente uma definição recursiva da função
-- isSorted :: Ord a => [a] -> Bool que testa se uma lista está ordenada por ordem crescente.
-- Porexemplo, isSorted [1,2,2,3,4,5] corresponde a True, enquanto que isSorted [1,2,4,3,4,5]
-- corresponde a False.

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h1 : h2 : t) = h1 <= h2 && isSorted (h2 : t)

-- 34
-- Apresente uma definição recursiva da função
-- iSort :: Ord a => [a] -> [a] que calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função 
-- insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir 
-- ordenadamente esse elemento na lista.

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (h : t) | h < minList t = h : iSort t
              | otherwise  = minList t : iSort (delete (minList t) (h : t))

minList :: Ord a => [a] -> a
minList [] = undefined
minList [x] = x
minList (h : t) | h < minList t = h
                | otherwise = minList t

-- 35
-- Apresente uma definição recursiva da função
-- menor :: String -> String -> Bool que dadas duas strings, retorna True se e só se a primeira for menor do que a segunda,
-- segundo a ordem lexicográfica (i.e., do dicionário)
-- Por exemplo, menor "sai" "saiu" corresponde a True enquanto que menor "programacao" "funcional" corresponde a False

menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = True
menor (h1 : t1) (h2 : t2) = fromEnum (toLower h1) <= fromEnum (toLower h2) && menor t1 t2

toLower :: Char -> Char
toLower c | fromEnum c >= 65 && fromEnum c <= 90 = toEnum (fromEnum c + 32) :: Char
          | fromEnum c >= 97 && fromEnum c <= 122 = c
          | otherwise = undefined

-- 36
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas 
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um elemento pertence a um multi-conjunto.
-- Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True enquanto que 
-- elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False.

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x (h : t) | fst h == x = True
                   | otherwise  = elemMSet x t

-- 37
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas 
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- lengthMSet :: [(a,Int)] -> Int que calcula o tamanho de um multi-conjunto.
-- Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7.

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (h : t) = snd h + lengthMSet t

-- 38
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- converteMSet :: [(a,Int)] -> [a] que converte um multi-conjuto na lista dos seus elementos.
-- Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac".

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,0) : t) = converteMSet t
converteMSet ((x,n) : t) = x : converteMSet ((x,n - 1) : t)

-- 39
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas 
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta um elemento a um multi-conjunto.
-- Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2), (’a’,4), (’c’,2)].

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((h,n) : t) | x == h = (h,n + 1) : t
                         | otherwise = (h,n) : insereMSet x t


-- 40. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas 
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um elemento a um multi-conjunto. Se o elemento não existir, 
-- deve ser retornado o multi-conjunto recebido.
-- Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2), (’a’,4)].

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x (h : t) | x == fst h = t
                     | otherwise = h : removeMSet x t

-- 41
-- Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a. Considere ainda que nestas 
-- listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero. Defina a função
-- constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos.
-- Por exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)].

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h : t) = myReverse (insereMSet h (constroiMSet t))

-- 42
-- Apresente uma definição recursiva da função pré-definida
-- partitionEithers :: [Either a b] -> ([a],[b]) que divide uma lista de Eithers em duas listas.

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (Left h : t) = (h : fst (partitionEithers t), snd (partitionEithers t))
partitionEithers (Right h : t) = (fst (partitionEithers t), h : snd (partitionEithers t))

-- 43
-- Apresente uma definição recursiva da função pré-definida
-- catMaybes :: [Maybe a] -> [a] que colecciona os elementos do tipo a de uma lista.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x) : t) = x : catMaybes t
catMaybes (Nothing : t) = catMaybes t

-- Considere o seguinte tipo para representar movimentos de um robot.

data Movimento = Norte | Sul | Este | Oeste
                deriving Show

-- 44
-- Defina a função
-- posicao:: (Int,Int) -> [Movimento] -> (Int,Int) que, dada uma posição inicial (coordenadas) e uma lista de movimentos, 
-- calcula a posição final do robot depois de efectuar essa sequência de movimentos.

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (Norte : t) = posicao (x,y + 1) t
posicao (x,y) (Sul : t) = posicao (x,y - 1) t
posicao (x,y) (Este : t) = posicao (x + 1,y) t
posicao (x,y) (Oeste : t) = posicao (x - 1,y) t

-- 45
-- Defina a função
-- caminho :: (Int,Int) -> (Int,Int) -> [Movimento] que, dadas as posições inicial e final (coordenadas) do robot, produz 
-- uma lista de movimentos suficientes para que o robot passe de uma posição para a outra.

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho p1 p2 | p1 == p2 = []
              | fst p1 < fst p2 = Este : caminho (fst p1 + 1, snd p1) p2
              | fst p1 > fst p2 = Oeste : caminho (fst p1 - 1, snd p1) p2
              | snd p1 < snd p2 = Norte : caminho (fst p1, snd p1 + 1) p2 
              | otherwise = Sul : caminho (fst p1, snd p1 - 1) p2

-- 46
-- Defina a função
-- vertical :: [Movimento] -> Bool que, testa se uma lista de movimentos só é composta por movimentos verticais (Norte ou Sul).

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Oeste : t) = False
vertical (Este : t) = False
vertical (Norte : t) = vertical t
vertical (Sul : t) = vertical t

-- Considere o seguinte tipo para representar a posi ̧c ̃ao de um robot numa grelha.

data Posicao = Pos Int Int
                deriving Show

-- 47
-- Defina a função
-- maisCentral :: [Posicao] -> Posicao que, dada uma lista não vazia de posições, determina a que está mais perto da origem
-- (note que as coordenadas de cada ponto são números inteiros).

maisCentral :: [Posicao] -> Posicao
maisCentral [] = undefined 
maisCentral [x] = x
maisCentral (p1 : p2 : t) | distanceSquared p1 (Pos 0 0) <= distanceSquared p2 (Pos 0 0) = maisCentral (p1 : t)
                          | otherwise = maisCentral (p2 : t)

distanceSquared :: Posicao -> Posicao -> Int
distanceSquared (Pos x1 y1) (Pos x2 y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

-- 48
-- Defina a função
-- vizinhos :: Posicao -> [Posicao] -> [Posicao] que, dada uma posição e uma lista de posições, selecciona da lista as 
-- posições adjacentes à posição dada.

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos x (h : t) | vizinho x h = h : vizinhos x t
                   | otherwise  = vizinhos x t

vizinho :: Posicao -> Posicao -> Bool
vizinho p1 p2 = distanceSquared p1 p2 > 0 && distanceSquared p1 p2 <= 2 

-- 49
-- Defina a função
-- mesmaOrdenada :: [Posicao] -> Bool que testa se todas as posições de uma dada lista têm a mesma ordenada.

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1) : (Pos x2 y2) : t) | y1 == y2 = mesmaOrdenada (Pos x1 y1 : t)
                                              | otherwise = False

-- Considere o seguinte tipo para representar o estado de um semáforo.

data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

-- 50
-- Defina a função
-- interseccaoOK :: [Semaforo] -> Bool que testa se o estado dos semáforos de um cruzamento é seguro, i.e., não há 
-- mais do que um semáforo não vermelho.