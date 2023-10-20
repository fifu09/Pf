-- Conversiones
    -- intToDigit 3 devuelve el carácter '3'.
    -- digitToInt '3' devuelve el entero 3.
    -- ord '3' o 'a' devuelve numero en ASCII.
    -- fromEnum 'a' devuelve 97
    -- toEnum 97 devuelva 'a'
    -- show 123 devuelve la cadena "123".
    -- read "123" :: Int convertirá la cadena "123" en el entero 123.
    -- toUpper 'a' devuelve 'A'.
    -- toLower 'A' devuelve 'a'.
-- Recorrer Listas
    -- head [2,4,6,1] = 2
    -- tail [2,4,6,1] = [4,6,1]
    -- last [2,4,6,1] = 1
    -- init [2,4,6,1] = [2,4,6]
    -- length [2,4,6,1] = 4
    -- null [2,4,6,1] = False
    -- reverse [2,4,6,1] = [1,6,4,2]
    -- [8,9,1,5] ++ [7,1,2,6] = [8,9,1,5,7,1,2,6]
    -- take 2 [8,9,1,5,4] = [8,9] 
    -- drop 2 [8,9,1,5,4] = [1,5,4]
    -- zip [0..2] "epa" = [(0,'e'),(1,'p'),(2,'a')]
    -- unzip [(0,'e'),(1,'p'),(2,'a')] = [0,1,2] "epa"
    -- filter 
        -- par x = (mod x 2 == 0) ESTO SERIA P DE ABAJO QUE ES UN PREDICADO
        -- filter p [] = []
        -- filter p (x:xs) = if p x then x:filter p xs else filter p xs
        -- implementacion: filter par [2,3,5,6,7,8,9,0]
    -- map
        -- map f [] = []
        -- map f (x:xs) = f x : map f xs
    -- takeWhile par [8,2,1,6,3,10] = [8,2]
    -- dropWhile par [8,2,1,6,3,10] = [1,6,3,10]
    -- span par [8,2,1,6,3,10] = ([8,2], [1,6,3,10])
    -- Elementos positivos: 
            -- positList (x:xs)
            --     | x>0 = x:positList xs
            --     | otherwise = positList xs
-- Tuplas
    -- fst (x,y) devuelve x (SOLO PARA TUPLAS PARES)
    -- snd (x,y) devuelve y (SOLO PARA TUPLAS PARES)
-- COMPRUEBA EL ESTADO DEL PARAMETRO

-- Operadores 
    -- case e of
        -- paridad x = case mod x 2 of
        --     0 -> "par"
        --     1 -> "impar"
    -- fold (aplica operacion a todos los elementos devolviendo 1)
        -- foldl (+) 0 [8,2,6,10] = 26
        -- foldr (∗) 1 [3,2,5,10] = 300
    -- scan (parecido a fold pero devuelve lista con todos los valores
            -- por los que ha pasado para llegar al valor final)
        -- scanl (+) 0 [1..5] => [0,1,3,6,10,15]
        -- scanr (+) 0 [1..5] => [15,14,12,9,5,0]

-- ListasIntensionales
    -- Notacion alternativa para usar en lugar de filter y map
    -- [expresion | cualificador1 , cualificador2 , ...]
    -- Cada cualificador es un filtro o un generador
    -- Ej: [x∗x | x <− [1..7], mod x 2 == 1] => [1,9,25,49]
    --   Expresion  Generador      Filtro


isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList l = False

listaNoAplanada :: [[Integer]]
listaNoAplanada = [[0,1],[3,4,5]]
listaAplanada :: [Integer]
listaAplanada = concat listaNoAplanada

listaPrimera :: String
listaPrimera = "abcde" -- vale con listas ej: [1,2,3,4,5]
tresPrimerasLetras :: [Char]
tresPrimerasLetras = take 3 listaPrimera

nombres :: [String]
nombres = ["Pepe", "Pepa", "Poper"]
numerosTlf :: [Integer]
numerosTlf = [645,456,875]
listasCombinadas :: [(String, Integer)]
listasCombinadas = zip nombres numerosTlf

-- Definir tipos de datos

    -- desde simples
type Dinero = Integer
type Complex = (Double, Double)
    -- Enum
data DiaSemana = Lunes | Martes | Miercoles deriving(Eq, Ord, Show, Read)
        -- Con deriving le damos ciertas propiedades, se pueden comparar, permitir
        -- la ordenacion, se pueden convertir en cadena y se pueden imprimir o interpretar
    -- Tipo union -> juntar 2 tipos de datos
data CadenaONumero = Num | Char
    -- Producto -> Tipo de tipos
    -- Ej: menu del dia es un conjunto de menus que son 3 integers
data MenuDelDia = Menu Integer Integer Integer
menuDelDia = Menu 1 2 4

-- Dibujo es una lista de listas de caracteres, 
type Dibujo = [[Char]]
printDibujo :: Dibujo -> IO()
-- putStr printea el contenido de la derecha por consola (detectando caracteres especiales como los saltos de linea)
-- concat concatena todas las listas de listas de caracteres en una sola para ser printeada por putStr
-- map anade a cada elemento de la lista un salto de lineas
printDibujo = putStr . concatMap (++"\n")

-- Esto hace lo mismo que la funcion anterior pero usando la funcion unlines que se encarga tanto de juntar las cadenas como de poner salto de linea entre ellas
printDibujo2 :: [String] -> IO ()
printDibujo2 = putStr . unlines

-- Definicion de funciones
    -- simple
doble :: Num a => a -> a
doble x = x + x
sumdo :: Num a => a -> a -> a
sumdo x y = x + doble y

    -- condicional
absoluto :: (Ord a, Num a) => a -> a
absoluto x = if x>=0 then x else -x

    -- por casos
signo x
    | x > 0 = 1
    | x == 0 = 0
    | x < 0 = -1

    -- recursivas
factorial x
    | x < 0 = error "dato negativo"
    | x == 0 = 1
    | otherwise = x * factorial (x-1)

    -- locales usando where
g x y = (a+b) * (a-b)
    where
    a = x + y
    b = x * y
    -- en lugar de usar: g x y = ((x + y) + (x ∗ y)) ∗ ((x + y) - (x ∗ y)) 

-- Composicion de funciones (Currificcion)
-- Hacemos el factorial del factorial sin necesidad de usar las funciones 2 veces o repetir parametros
factorialDeFactorial = factorial.factorial 
