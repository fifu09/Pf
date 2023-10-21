import Data.Char (intToDigit, digitToInt)
-- 1. Define una función quitaUno que quite de una lista dada la primera aparición (si existe alguna) de
-- un elemento dado. Ejemplo: quitaUno 5 [1,3,5,7,3,5,8] = [1,3,7,3,5,8]

quitaUno _ [] = []
quitaUno n (x : xs) = if x /= n then x : quitaUno n xs else xs

-- 2. Define una función quitaRep que dada una lista elimine los elementos repetidos.
-- Ejemplo: quitaRep [1,3,5,7,3,5,8] = [1,3,5,7,8]

quitaRep [] = []
quitaRep (x : xs) = x : quitaRep (filter (/= x) xs)

-- OTRA FORMA DE HACERLO
-- quitarTodos _ [] = []
-- quitarTodos n (x:xs) = if x/=n then x:quitaUno n xs else quitaUno n xs
-- quitaRep [] = []
-- quitaRep (x:xs) = x:quitaRep l
--     where l = quitarTodos x xs

-- 3. Usando quitaUno, define una función dif que realice la diferencia de dos listas. Ejemplo: dif
-- [1,2,3,7,4,2,8,4] [3,5,2,4,2] = [1,7,8,4]

dif (x : xs) [] = x : xs
dif [] (y : ys) = y : ys
dif [] [] = []
dif (x : xs) (y : ys) = dif (quitaUno y (x : xs)) ys

-- 4. Usando dif, define una función perm que decida si una lista es permutación de otra. Ejemplos:
-- perm “abc” “acb” = True perm [1,2,2,4] [2,4,1] = False

perm l1 l2 = null (dif l1 l2)

-- 5. Usando perm y filter, define una función sonPermDe1 que, dada una lista de listas xss , obtenga
-- la sublista de xss formada por aquellos elementos que son permutaciones de la cabeza de xss.
-- Ejemplo: sonpermde1 ["abc", "ac", "bca", "bbc", "cab"] = [ “abc”, “bca”, “cab”]
sonPermDe1 [] = error "Lista vacia"
sonPermDe1 (x : xs) = x : filter (perm x) xs

-- 6. Usando alguna versión de fold, define una función aDecimal que convierta una lista de dígitos en
-- el número decimal que representa. Define su función inversa aDigitos.
-- Ejemplos: aDecimal [9,6,3,8] = 9638 aDigitos 9638 = [9,6,3,8]

anadir2Elementos a b = if a >= 0 then a * 10 + b else a * 10 - b

aDecimal [] = 0
aDecimal l = foldl anadir2Elementos 0 l

aDigitos :: Int -> [Int]

aDigitos x
    | null a = error "Null"
    | length a == 1 = [x]
    | otherwise = read [head a] : aDigitos (read (tail a)::Int)
  where
    a = show x

-- 7. Define las funciones decimalAbinario y binarioAdecimal para transformar un número decimal
-- a binario y viceversa.
-- Ejemplos: decimalAbinario 13 = 1101 binarioAdecimal 1101 = 13

decimalABinario :: Int -> Int
decimalABinario 0 = 0
decimalABinario x = read (show (decimalABinario cociente) ++ show resto) :: Int
    where
        resto = mod x 2
        cociente = div x 2

binarioAdecimal :: Int -> Int
binarioAdecimal x 
    | n > 1 = (2^(n-1)) + binarioAdecimal nextNumbers
    | n == 1 = 1
    | otherwise = 0
    where  
        n = length (show x)
        nextNumbers = read (tail (show x)) :: Int



-- 8. Define un predicado que decida si una lista está ordenada.
-- Ejemplos: ordenada “ayu” = False ordenada [1,3,7,12] = True




-- 9. Usando takeWhile y dropWhile, define la función palabras que dada una frase (string) obtenga
-- la lista de palabras (lista de strings) de la frase.
-- Ejemplo: palabras “ Estoy en el laboratorio” = [“Estoy”, “en”, “el”, “laboratorio”]
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
-- Ejemplos: sublista “palma” “tapa la mesa” = True
-- subsecuencia “palma” “tapa la mesa” = False
-- 14. Evalúa la siguiente expresión, descrita mediante una lista intensional:
-- [j | k <− [1,-1, 2, -2], k>0, j <− [1.. k]]
-- 15. Dado un string, define una función diag que devuelva cada carácter del string en una línea y en
-- diagonal. Ejemplo:
-- ? diag “haskell”
-- h
--  a
--  s
--  k
--  e
--  l
--  l

-- 16. Define una función repLong que dado un string devuelva tantas copias del string como su
-- longitud y una en cada línea. Ejemplo:
-- ? repLong “hugs”
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