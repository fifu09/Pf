-- PROGRAMACION FUNCIONAL 2023/2024
-- LISTA DE EJERCICIOS 3
-- Ejercicios sobre inferencia de tipos
-- NOTA import Data.Char (para usar funciones predefinidas sobre Char como isDigit, ord, chr, …)
-- ord:: Char ->Int es similar a fromEnum::Enum α => α -> Int
-- chr:: Int-> Char es similar a toEnum::Enum α => Int -> α
import Data.Char (isDigit, ord, chr);

-- 1. Da el tipo (más general posible) de las siguientes funciones y calcula el resultado de la evaluación de
-- las expresiones que se piden. Copia después las definiciones en un fichero, cárgalo en memoria y
-- comprueba qué responde el sistema tanto respecto al tipo más general inferido como a los resultados
-- de las expresiones.

--iden :: p -> p
--iden x = x -- SHOW
    -- pregunta el tipo con :t iden y evalúa las expresiones:
    -- ? iden 'a'
        -- iden 'a' :: Char
    -- ? iden 9

-- aplicarPar :: (t -> a, t -> b) -> t -> (a, b)
-- aplicarPar (f,g) x = (f x, g x)
    -- pregunta el tipo con :t aplicarPar

-- digitoCod :: f -> (Bool, Int)
-- digitoCod = aplicarPar (isDigit, ord)
    -- pregunta :t digitoCod y evalúa las expresiones:
    -- ? digitoCod '5'
    -- ? digitoCod 'b'

-- iguales :: Eq a => a -> a -> [Char]
-- iguales x y
    -- | x /= y = "no"
    -- | otherwise = "si"
    -- pregunta :t iguales y evalúa las expresiones:
    -- ? iguales 'a' 'a'
    -- ? iguales "hola" "adios"

-- elMenor :: Ord a => a -> a -> a
-- elMenor x y
    -- | x<y = x
    -- | otherwise = y
    -- pregunta :t elMenor y evalúa las expresiones:
    -- ? elMenor "hola" "adios"
    -- ? elMenor 'f' 'h'
    -- además comprueba los errores de tipo al evaluar:
    -- ? elMenor 'a' "hola"
    -- ? elMenor isDigit isUpper
    -- ? elMenor digitToInt isUpper


-- 2. Da el tipo (más general posible) de las siguientes funciones. Copia después las definiciones en un
-- fichero, cárgalo en memoria y comprueba qué responde el sistema como tipo inferido.
    -- f1 x y = chr x == y
        -- f1 :: Int -> Char -> Bool
    -- f2 x y = [length y]++y
        -- f2 :: a -> [Int] -> [Int]
    -- f3 x y = fst x > y
        -- f3 :: Ord a => (a,b) -> a -> Bool
    -- f4 x y = y x : x
        -- f4 :: [a] -> ([a] -> a) -> [a]
    -- f5 (x,y) = if x=='a' then True else y
        -- f5 :: (Char,Bool) -> Bool

-- 3. Da el tipo (más general posible) de las siguientes funciones y calcula el resultado de la evaluación de
-- las expresiones que se piden. Copia después las definiciones en un fichero, cárgalo en memoria y
-- comprueba qué responde el sistema tanto respecto al tipo más general inferido como a los resultados
-- de las expresiones.
    -- por2mas1 x = 2*x +1
        -- por2mas1 :: Num a => a -> a
    -- replica f x = f (f x)
        -- replica :: (a -> a) -> a -> a
    -- h1 (x,y) = if x <5.5 then x+1 else y
        -- h1 :: (Ord a, Fractorial a) a => a -> a -> a
    -- Expresiones a evaluar:
    -- ? replica por2mas1 3
    -- ? h1 (6, 1/0)
    -- ? h1 (4, 1/0)


-- 4. Dadas las siguientes definiciones:
    -- f (u,v) n
        -- | u == v = n+1
        -- | otherwise = n
    -- g xs = foldr f 0 xs
    -- h xs ys = g (zip xs ys)
    -- (a) Da el valor de la expresión h [3,5,1,2,4][8,5,3,1,4,7] ¿Qué hace la función h?
    -- (b) Da el tipo (más general posible) de las tres funciones. Copia después las definiciones en un
    -- fichero, cárgalo en memoria y comprueba qué responde el sistema como tipo inferido.
        -- f :: (Eq a, Num n) => (a,a) -> n -> n
        -- g :: (Eq a, Num n) => [(a,a)] -> n
        -- h :: (Eq a, Num n) => [a] -> [a] -> n


    -- Ejercicios sobre impresión en pantalla

-- 5. Suponer que representamos una fecha mediante un triple de números (d,m,a), con d el día, m el mes
-- y a el año. Define una función muestraFecha que aplicada a una fecha la muestre en pantalla de la
-- forma:
    -- ? muestraFecha (3,11,2003)
    -- 3 de Noviembre de 2003
meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Nombiembre", "Diciembre"]
getMes :: Int -> [String] -> String
getMes n meses
    | n<1 || n>12 = error "Mes incorrecto"
    | length meses == n = last meses
    | otherwise = getMes n (init meses)

anoBisiesto a
    | mod a 4 == 0 && mod a 100 /=0 = True
    | mod a 100 == 0 && mod a 400 == 0 = True
    | mod a 100 == 0 && mod a 400 /= 0 = False
    | otherwise = False
condicionBisiesto :: (Int, Int, Int) -> Bool
condicionBisiesto (d, m, a)
    | m == 2 && d>29 = False
    | m == 2 && d == 29 && anoBisiesto a = True
    | m == 2 && d == 29 && not (anoBisiesto a) = False
    | otherwise = False
muestraFecha :: (Int, Int, Int) -> String
muestraFecha (d,m,a)
    | not (condicionBisiesto (d, m, a)) = error "No cumple con bisiesto"
    | otherwise = show d ++ " de " ++ getMes m meses ++ " de " ++ show a

-- 6. Define una función tablaMult que, dado un número n (entre 1 y 9) muestre en pantalla la tabla de
-- multiplicar del número n de la forma siguiente:
    -- ? tablaMult 3
    -- 1 * 3 = 3
    -- 2 * 3 = 6
    -- 3 * 3 = 9
    -- . . . . . . . . . .
    -- 10 * 3 = 30
lista10 = [1..10]
dibujar :: Int -> [Int] -> [String]
dibujar _ [] = []
dibujar n lista= (show (head lista) ++ " * " ++ show n ++ " = " ++ show ( head lista*n)++"\n") : dibujar n (tail lista)
tablaMult n
     | n>10 || n<1 = error "Numero incorrecto"
     | otherwise = putStr (concat (dibujar n lista10))

-- 7. Define una función todasTablasMult que muestre en pantalla todas las tablas de multiplicar (desde
-- la tabla del 1 a la tabla del 9) de la forma siguiente:
    -- ? todasTablasMult
    -- (*1) 1 2 3 4 5 6 7 8 9 10
    --  = 1 2 3 4 5 6 7 8 9 10
    -- (*2) 1 2 3 4 5 6 7 8 9 10
    --  = 2 4 6 8 10 12 14 16 18 20
    -- (*3) 1 2 3 4 5 6 7 8 9 10
    --  = 3 6 9 12 15 18 21 24 27 30
    -- . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    -- (*9) 1 2 3 4 5 6 7 8 9 10
    --  = 9 18 27 36 45 54 63 72 81 90

multiplos :: Int -> [Int] -> [String]
multiplos _ [] = []
multiplos n lista
    | length lista == 1 = (espacio++show (n*head lista)++"\n\n"):multiplos n (tail lista)
    |otherwise = (espacio++show (n*head lista)):multiplos n (tail lista)
    where 
        tamanoMult = length (show (n*head lista))
        espacio 
            | tamanoMult == 3 = " "
            | tamanoMult == 2 = "  "
            | otherwise = "   "
todasTablasMultAux :: Int -> String
todasTablasMultAux n = concat(("(*"++show n++") 1   2   3   4   5   6   7   8   9  10\n =") : multiplos n [1..10])
todasTablasMultAux2 :: Int -> [String]
todasTablasMultAux2 11 = []
todasTablasMultAux2 n= todasTablasMultAux n : todasTablasMultAux2 (n+1)

todasTablasMult = putStr(concat(todasTablasMultAux2 1))