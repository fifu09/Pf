import Data.Char (isDigit)
-- LISTA DE EJERCICIOS 2

-- 1. Define una función quitaUno que quite de una lista dada la primera aparición (si existe alguna) de
-- un elemento dado. Ejemplo: quitaUno 5 [1,3,5,7,3,5,8] = [1,3,7,3,5,8]
quitaUno:: Eq a => a -> [a] -> [a]
quitaUno _ [] = []
quitaUno x (y:ys)
  | x == y = ys
  | otherwise = y: quitaUno x ys

-- 2. Define una función quitaRep que dada una lista elimine los elementos repetidos.
-- Ejemplo: quitaRep [1,3,5,7,3,5,8] = [1,3,5,7,8]
quitaRep :: [Int] -> [Int]
quitaRep [] = []
quitaRep (x:xs) = x: quitaRep (filter (/=x) xs)

-- 3. Usando quitaUno, define una función dif que realice la diferencia de dos listas. Ejemplo: dif
-- [1,2,3,7,4,2,8,4] [3,5,2,4,2] = [1,7,8,4]
dif :: Eq a => [a] -> [a] -> [a]
dif (x:xs) [] = x:xs
dif (x:xs) (y:ys) = dif (quitaUno y (x:xs)) ys

-- 4. Usando dif, define una función perm que decida si una lista es permutación de otra. Ejemplos:
-- perm "abc" "acb" = True perm [1,2,2,4] [2,4,1] = False

personalSort :: Ord a => [a] -> [a]
personalSort (x:xs)
    | length (x:xs)==1 = [x]
    | otherwise = max : personalSort (quitaUno max (x:xs))
    where max = maximum (x:xs)

perm :: Ord a => [a] -> [a] -> Bool
perm l1 l2 = personalSort l1 == personalSort l2

-- 5. Usando perm y filter, define una función sonPermDe1 que, dada una lista de listas xss , obtenga
-- la sublista de xss formada por aquellos elementos que son permutaciones de la cabeza de xss.
-- Ejemplo: sonpermde1 [ "abc", "ac", "bca", "bbc", "cab"] = [ "abc", "bca", "cab"]
sonpermde1 :: Ord a => [[a]] -> [[a]]
sonpermde1 list = filter (perm (head list)) list

-- 6. Usando alguna versión de fold, define una función aDecimal que convierta una lista de dígitos en
-- el número decimal que representa. Define su función inversa aDigitos.
-- Ejemplos: aDecimal [9,6,3,8] = 9638 aDigitos 9638 = [9,6,3,8]
aDecimalAux :: [Int] -> [Int]
aDecimalAux [] = []
aDecimalAux list = (head list * (10^tamano)):aDecimalAux (tail list)
    where tamano = length list -1
aDecimal :: [Int] -> Int
aDecimal list =  foldl (+) 0 (aDecimalAux list)
aDigitos :: Int -> [Int]
aDigitos x = map (\x-> read [x]::Int) list
    where list = show x
-- 7. Define las funciones decimalAbinario y binarioAdecimal para transformar un número decimal
-- a binario y viceversa.
-- Ejemplos: decimalAbinario 13 = 1101 binarioAdecimal 1101 = 13
decimalAbinarioAux n
    | div n 2 == 0 = [0,0]
    | div n 2 == 1 = [1,1]
    | otherwise = mod n 2 : decimalAbinarioAux (div n 2)
decimalAbinario :: Int -> Int
decimalAbinario = aDecimal.reverse.decimalAbinarioAux
binarioAdecimal :: Int -> Int
binarioAdecimal bin
    | length list==1 = head list
    | otherwise = 2^(length list - 1)+binarioAdecimal (aDecimal (tail list))
    where
        list = aDigitos bin

-- 8. Define un predicado que decida si una lista está ordenada.
-- Ejemplos: ordenada "ayu" = False ordenada [1,3,7,12] = True
ordenada list = list ==  reverse (personalSort list)

-- 9. Usando takeWhile y dropWhile, define la función palabras que dada una frase (string) obtenga
-- la lista de palabras (lista de strings) de la frase.
-- Ejemplo: palabras " Estoy en el laboratorio" = ["Estoy", "en", "el", "laboratorio"]

palabras :: String -> [String]
palabras [] = []
palabras (x:xs) | x==' ' = palabras xs
--palabras  (x:xs) = takeWhile (/=' ') (x:xs)

-- 10. Define la función posiciones que devuelva las posiciones de un elemento dado en una lista dada.
-- Ejemplo: posiciones 8 [8,9,5,8,2] = [0,3]

-- 11. Define una función paraTodo que dado un predicado y una lista decida si todos sus elementos
-- satisfacen el predicado, y una función existe que dado un predicado y una lista decida si alguno de
-- sus elementos satisface el predicado.
-- Compáralas con las predefinidas: all, any :: (α -> Bool) -> [α] -> Bool

-- 12. Define una función permutar que devuelva todas las permutaciones de una lista dada.
-- Ejemplo: permutar [2,8,3] = [[2,8,3],[8,2,3],[8,3,2],[2,3,8],[3,2,8],[3,8,2]]

-- 13. Una lista es una sublista de otra si los elementos de la primera aparecen en la segunda en el
-- mismo orden. Una lista es una subsecuencia de otra si aparece en ella como secuencia de elementos
-- contiguos. Define las funciones sublista y subsecuencia.
-- Ejemplos: sublista "palma" "tapa la mesa" = True
-- subsecuencia "palma" "tapa la mesa" = False

-- 14. Evalúa la siguiente expresión, descrita mediante una lista intensional:
-- [j | k <− [1,-1, 2, -2], k>0, j <− [1.. k]]

-- 15. Dado un string, define una función diag que devuelva cada carácter del string en una línea y en
-- diagonal. Ejemplo:
-- ? diag "haskell"
-- h
--  a
--  s
--  k
--  e
--  l
--  l


-- 16. Define una función repLong que dado un string devuelva tantas copias del string como su
-- longitud y una en cada línea. Ejemplo:
-- ? repLong "hugs"
-- hugs
-- hugs
-- hugs
-- hugs
-- Los siguientes problemas deben resolverse empleando LISTAS INTENSIONALES

-- 17. Da definiciones alternativas a las funciones de los ejercicios 10, 11 y 12 utilizando listas
-- intensionales.

-- 18. Usar elem y listas intensionales para definir la función intersec:: [α]->[α]->[α], tal que intersec
-- xs ys obtenga la lista de todos los elementos de xs que también están en ys.

-- 19. Usar length para definir la función numVeces que calcule el número de apariciones de un
-- elemento dado en una lista dada. Ejemplo: numVeces 8 [8,9,5,8,2]  2.

-- 20. Define una función que, dadas dos listas, decida si en ambas aparecen exactamente los mismos
-- elementos (el número de apariciones es irrelevante).

-- 21. Usar map, filter y zip para definir una función que, dada una lista, obtiene la lista de todos sus
-- elementos cuya posición es impar (recuerda que la primera posición es 0).

-- 22. Usar and y zip para definir una función que decida si todos los elementos de una lista son
-- iguales.