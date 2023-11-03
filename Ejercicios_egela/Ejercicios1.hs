import Data.Char (isDigit, intToDigit)
import Data.List ( (\\) )
-- LISTA DE EJERCICIOS 1

-- 1. Dí si son bien formadas cada una de las siguientes expresiones. En caso afirmativo da su valor y en
-- caso negativo dí de qué clase es el error (sintáctico, de tipos, …):
-- (3== - - 3 ) && True (3 == -3 ) && True
-- False && (‘a’ == True)
-- x == True || x == False
-- False == (1<3)

-- 2. ¿Para qué argumentos las siguientes funciones devuelven True?
-- (==9).(2+).(7*)
-- (3>).(`mod` 2)

-- 3. Define funciones para calcular el área y el perímetro de un círculo con un radio r dado (la constante
-- pi de Haskell indica el número π).
perimetro r = 2*pi*r
area r = pi*(r^2)

-- 4. Define la función agregar que dados dos números enteros x e y, compruebe si y es un dígito y
-- en caso afirmativo lo “pegue a la derecha” de x. Ejemplo: agregar 146 3 = 1463
agregar:: Int -> Int -> Int
agregar x y
    | isDigit (intToDigit y) = x*10+y
    | otherwise = error "Y no es digito"

-- 5. Define una función sumcuad que tome tres números enteros y devuelva la suma de los cuadrados
-- de los dos mayores.
sumcuad :: Int -> Int -> Int -> Int
sumcuad a b c = firstMax + maximum ([a,b,c]\\[firstMax])
    where firstMax = maximum [a,b,c]

-- 6. Definir una función divMod::(Int,Int)->(Int,Int) que, dados dividendo y divisor,
-- devuelva el par formado por la división y el módulo de la división entera.
divMod2::(Int,Int)->(Int,Int)
divMod2 (a,b) = (div a b, mod a b)

-- 7. Define una función sigLetra::Char->Char que dada una letra del alfabeto devuelva la
-- siguiente letra (asumir que ‘A’ sigue a ‘Z’).
sigLetra::Char->Char
sigLetra x
    | x == 'z' = 'a'
    | x == 'Z' = 'A'
    | otherwise = toEnum (fromEnum x + 1)

-- 8. Define una función digitoVal::Char->Int que convierta un carácter dígito a su
-- correspondiente valor numérico.
digitoVal::Char->Int
digitoVal x
    | isDigit x = read [x] :: Int
    | otherwise = error "No es digito"

-- 9. Define la función prod::Int->Int->Int tal que prod n m devuelva el producto de los
-- números comprendidos entre n y m.
prod::Int->Int->Int
prod n m = product [n..m]

-- 10. Una fecha se puede representar por un triple de enteros (d, m, a) donde d es el día, m es el mes y a es
-- el año. Define una función edad que dadas dos fechas, la primera la fecha de nacimiento de una
-- persona P y la segunda la fecha actual, devuelva la edad de P mediante un número entero de años.
type Fecha = (Int,Int,Int)
edad :: Fecha -> Fecha -> Int
edad (d1,m1,a1) (d2,m2,a2)
    | anos < 0 = error "Fechas incorrectas"
    | otherwise = anos
    where anos = div ((a2 - a1)*365 + (m2 - m1)*30 + d2 - d1) 365

-- 11. Define un operador binario (|-|)::Bool->Bool->Bool que calcule el ó-exclusivo de dos
-- valores booleanos.
(|-|)::Bool->Bool->Bool
(|-|) a b
    | a == b = False
    | otherwise = True

-- 12. Define una función tresIgual que decida si sus tres argumentos son iguales. ¿De qué tipo es la
-- función? Evalúa las expresiones: tresIgual 4 5 4 y tresIgual ‘a’ ‘a’ ‘a’
tresIgual :: Eq a => a -> a -> a -> Bool
tresIgual a b c = a == b && b == c && c == a

-- 13. Define hms::Int->(Int,Int,Int) para calcular, a partir de un número total de segundos, la
-- hora en formato (horas, minutos, segundos). Ejemplo: hms 11720 = (3, 15, 20)
hms::Int->(Int,Int,Int)
hms x = (horas, minutos, segundos)
    where
        horas = div x 3600
        restoHoras = mod x 3600
        minutos = div restoHoras 60
        segundos = mod restoHoras 60

-- 14. Define una función triangulo::(Int,Int,Int)->String que, dados tres lados (x,y,z) con
-- x<=y<=z, devuelva error “no es triangulo” si los tres valores no pueden ser lados de un triángulo. Si
-- lo pueden ser, devolverá “escaleno” (resp. “isosceles” ó “equilatero”) si forman los lados de un
-- triángulo escaleno (resp. isósceles ó equilátero).
esTriangulo::(Int,Int,Int)->Bool
esTriangulo (x,y,z)
    | x+y<z || x+z<y || y+z<x = error "No es un triangulo"
    | otherwise = True
triangulo::(Int,Int,Int)->String
triangulo (x,y,z)
    | esTriangulo (x,y,z) && x==y && x==z = "Equilatero"
    | esTriangulo (x,y,z) && x/=y && x/=z && y/=z = "Escaleno"
    | otherwise = "Isosceles"

-- 15. Define versiones de las funciones (&&) y (||) usando patrones para el segundo argumento.
-- Define versiones usando patrones para ambos argumentos. Comprueba mediante ejemplos las
-- diferencias entre las distintas versiones.

