import Data.Char (isDigit, intToDigit, ord)
import Data.List (sort)
-- 3. Define funciones para calcular el área y el perímetro de un círculo con un radio r dado (la constante
-- pi de Haskell indica el número π).
diametro r = 2*r
rCuadrado r = r*r
perimetro = (*pi).diametro
area = (*pi).rCuadrado

-- 4. Define la función agregar que dados dos números enteros x e y, compruebe si y es un dígito y
-- en caso afirmativo lo “pegue a la derecha” de x. Ejemplo: agregar 146 3 = 1463

agregar x y
        | isDigit (intToDigit y) = read (show x ++ show y)::Int
        | otherwise = error "Y no es un digito"


-- 5. Define una función sumcuad que tome tres números enteros y devuelva la suma de los cuadrados
-- de los dos mayores.

segundoElementoOrdenado list = head (tail (sort list))
sumcuad x y z = minimum [x, y ,z] + segundoElementoOrdenado [x,y,z]^2 + maximum [x,y,z]^2

-- 6. Definir una función divMod::(Int,Int)->(Int,Int) que, dados dividendo y divisor,
-- devuelva el par formado por la división y el módulo de la división entera.

divMod2 a b = (div a b, mod a b)

-- 7. Define una función sigLetra::Char->Char que dada una letra del alfabeto devuelva la
-- siguiente letra (asumir que ‘A’ sigue a ‘Z’).

sigLetra a = if a == 'z'then 'a' else toEnum (fromEnum a + 1)

-- 8. Define una función digitoVal::Char->Int que convierta un carácter dígito a su
-- correspondiente valor numérico.
digitVal = ord

-- 9. Define la función prod::Int->Int->Int tal que prod n m devuelva el producto de los
-- números comprendidos entre n y m.
prod n m
    | m<n = error "Rango incorrecto"
    | m == n = m
    | n<m = n * prod (n+1) m

-- 10. Una fecha se puede representar por un triple de enteros (d, m, a) donde d es el día, m es el mes y a es
-- el año. Define una función edad que dadas dos fechas, la primera la fecha de nacimiento de una
-- persona P y la segunda la fecha actual, devuelva la edad de P mediante un número entero de años.

edad (aN,mN,dN) (aA,mA,dA)
    | aN<aA         = aA - aN
    | otherwise     = 0

-- 11. Define un operador binario (|-|)::Bool->Bool->Bool que calcule el ó-exclusivo de dos
-- valores booleanos.
orBooleano a b = a || b

-- 12. Define una función tresIgual que decida si sus tres argumentos son iguales. ¿De qué tipo es la
-- función? Evalúa las expresiones: tresIgual 4 5 4 y tresIgual ‘a’ ‘a’ ‘a’
tresIgual x y z = x == y && x == z && y == z 
-- 13. Define hms::Int->(Int,Int,Int) para calcular, a partir de un número total de segundos, la
-- hora en formato (horas, minutos, segundos). Ejemplo: hms 11720 = (3, 15, 20)
hms x = (horas, minutos, segundos) 
    where 
        restoHoras = mod x 3600
        minutos = div restoHoras 60
        segundos = mod restoHoras 60
        horas = div x 3600
-- 14. Define una función triangulo::(Int,Int,Int)->String que, dados tres lados (x,y,z) con
-- x<=y<=z, devuelva error “no es triangulo” si los tres valores no pueden ser lados de un triángulo. Si
-- lo pueden ser, devolverá “escaleno” (resp. “isosceles” ó “equilatero”) si forman los lados de un
-- triángulo escaleno (resp. isósceles ó equilátero).
sumaLados a b c = a+b>c && a+c>b && b+c>a
tipoTriangulo a b c 
    | a == b && b == c = "equilatero"
    | (a == b && b /= c)||(a == c && b /= c)||(c == b && a /= c) = "isosceles"
    | a /= b && b/=c && a /= c = "escaleno"
triangulo a b c 
    | sumaLados a b c = tipoTriangulo a b c 
    | otherwise = error "no es triangulo"

-- 15. Define versiones de las funciones (&&) y (||) usando patrones para el segundo argumento.
-- Define versiones usando patrones para ambos argumentos. Comprueba mediante ejemplos las
-- diferencias entre las distintas versiones.

