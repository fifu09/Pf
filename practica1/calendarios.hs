------------------------------------------------------------------
--  PRACTICA: CALENDARIO           --               PF  2023-24

--  Nombre:   Abderraouf Khedidji
------------------------------------------------------------------

-- Llamada principal es:  printCalendario c n
-- donde c = columnas (3 o 4) y
--       n = a�o cuyo calendario deseamos imprimir
------------------------------------------------------------------

module Calendarios where
import GHC.Float (leDouble)
import Data.List ((\\))

test1 = ["hola", "hola", "hola", "pepe"]

test2 = ["hola", "hola", "tengoDiferenteTamano"]

test3 = ["tengoDiferenteTamano", "hola", "hola", "hola"]

test4 = [["hola", "pepe"], ["pepe", "hola"], ["hola", "pepe"]]

test5 = [["marta", "hola"], ["sofia", "hola"], ["joseluis", "hsdola"]]

type Dibujo = [Linea] -- cada dibujo es una lista de lineas

type Linea = [Char] -- cada linea es una lista de caracteres

type Year = Int

type Columna = Int -- es 3 o 4

-- Para imprimir un dibujo en pantalla:
printDibujo :: Dibujo -> IO ()
printDibujo dib = do
  putStr "\n" -- putStr es la funci�n que imprime un String
  (putStr . concat . map (++ "\n")) dib

-- Imprime, con un numero de columnas, el calendario de un a�o:
-- printCalendario :: Columna ->  Year -> IO()
-- printCalendario c a = printDibujo (calendario c a)

-- Dibujo de un calendario (en c columnas) de un a�o dado:
-- calendario :: Columna -> Year -> Dibujo
-- calendario c  =  bloque c . map dibujomes . meses

---------------------------------------------------
--  Define las siguientes funciones sobre dibujos:
---------------------------------------------------

-- comprueba que las lineas de un dibujo tienen igual longitud,
-- debe dar un mensaje de error si el dibujo es vac�o ([]).
dibEsCorrecto :: Dibujo -> Bool
dibEsCorrecto [] = error "Error en dibEsCorrecto => Lista vacia"
dibEsCorrecto (x : xs)
  | null x = True
  | length xs == 1 = d1 == d2
  | d1 == d2 = dibEsCorrecto xs
  | otherwise = False
  where
    d1 = length x
    d2 = length (head xs)

------------------------------------------------------------------

-- comprueba que los dibujos de la lista dada son correctos y
-- ademas tienen todos las mismas dimensiones.
listaDibCorrectos :: [Dibujo] -> Bool
listaDibCorrectos = dibEsCorrecto.concat

------------------------------------------------------------------

-- Pre: dib es un dibujo correcto.
-- alto dib da la altura de dib.
alto :: Dibujo -> Int
alto l =
  if dibEsCorrecto l
    then length l
    else error "Error en alto => dibujoIncorrecto"

mismaAltura d1 d2 = alto d1 == alto d2

------------------------------------------------------------------

-- Pre: dib es un dibujo correcto.
-- ancho dib da la anchura de dib.
ancho :: Dibujo -> Int
ancho (x : xs) =
  if dibEsCorrecto (x : xs)
    then length x
    else error "Error en ancho => dibujoIncorrecto"

mismaAnchura d1 d2 = ancho d1 == ancho d2

------------------------------------------------------------------

-- Precondicion: los dibujos d1 y d2 tienen la misma anchura.
-- sl1obre d1 d2 pone el dibujo d1 sobre el dibujo d2.
sobre :: Dibujo -> Dibujo -> Dibujo
sobre d1 d2 =
  if mismaAnchura d1 d2
    then d1 ++ d2
    else error "Error en sobre => no tienen la misma anchura"

------------------------------------------------------------------

-- Precondicion: los dibujos d1 y d2 tienen la misma altura.
-- alLado d1 d2 da un dibujo con d1 a la izquierda de d2.
alLadoAux :: Dibujo -> Dibujo -> Dibujo
alLadoAux d1 d2
  | null d1 || null d2 = []
  | otherwise = (head d1 ++ head d2) : alLadoAux (tail d1) (tail d2)

alLado :: Dibujo -> Dibujo -> Dibujo
alLado d1 d2 =
  if mismaAltura d1 d2
    then alLadoAux d1 d2
    else error "Error en sobre => no tienen la misma altura"

------------------------------------------------------------------

-- apila s da el dibujo obtenido apilando todos los elementos de s
--         (el primero de s queda en la cima de la pila).
-- Si s no es una lista de dibujos correctos debe dar error.
apilar :: [Dibujo] -> Dibujo
apilar [] = []
apilar s =
  if listaDibCorrectos s
    then head s ++ apilar (tail s)
    else error "Error en apilar => dibujos incorrectos"

------------------------------------------------------------------

-- extiende s da el dibujo obtenido al extender todos los elementos
--            de s (el primero de s queda el m�s a la izquierda).
-- Si s no es una lista de dibujos correctos debe dar error.

extender :: [Dibujo] -> Dibujo
extender [] = []
extender s = if listaDibCorrectos s
  then foldl1 alLado s
  else error "Error en extender => dibujos incorrectos"

------------------------------------------------------------------

-- Precondicion: al>0 && an>0.
-- dibBlanco (al,an) devuelve el dibujo de caracteres blancos con
--                   altura al y anchura an
dibBlanco :: (Int,Int) -> Dibujo
dibBlanco (al,an) = if al>0 && an>0
  then [concat [" " | x<-[1..al]] | y<-[1..an]]
  else error "Error en dibBlanco => parametros negativos"

------------------------------------------------------------------


-- bloque n lisDib es el dibujo formado al agrupar de n en n los
--               dibujos de lisDib, extender cada sublista
--               y luego apilar los resultados.
difDibujos :: [Dibujo] -> [Dibujo] -> [Dibujo]
difDibujos l1 l2 = l1 \\ l2

obtenerNElementos :: Int -> [Dibujo] -> [Dibujo]
obtenerNElementos x d
  | x == 0 = []
  | null d = []
  | otherwise = head d : obtenerNElementos (x-1) (tail d)

testBloque = [["d1", "d2","d3", "d4"], ["d5", "d6", "d7", "d8"], ["d9", "d1", "dA", "dB"]]
bloque :: Int -> [Dibujo] -> Dibujo
bloque 0 _ = []
bloque _ [] = []
bloque x d =  concat (apilar dibujosObtenidos) : [concat (bloque x restoDeDibujos)]
  where
    dibujosObtenidos = obtenerNElementos x d
    restoDeDibujos = difDibujos d dibujosObtenidos

-- otras funciones auxiliares sobre dibujos que se necesiten:

------------------------------------------------------------------
-- Define constantes y funciones para calcular y dibujar los meses
------------------------------------------------------------------

-- meses ::  Year -> [(String, Year, Int, Int)]
-- meses n devuelve una lista de 12 elementos con los datos
--         relevantes de cada uno de los meses del a�o n:
--         (nombre_mes, n, primer_d�a_mes, longitud_mes)

------------------------------------------------------------------

-- dibujomes ::(String, Year, Int, Int) -> Dibujo
-- dibujomes (nm,a,pd,lm) devuelve un dibujo de dimensiones 10x25
-- formado por el titulo y la tabla del mes de nombre nm y a�o a.
-- Necesita como par�metros: pd=primer dia y lm=longitud del mes.

------------------------------------------------------------------

ene1 :: Year -> Int
ene1 a = mod (a + div (a - 1) 4 - div (a - 1) 100 + div (a - 1) 400) 7

-- ene1 a devuelve el dia de la semana del 1 de enero del a�o a
--        siendo 1=lunes, 2=martes, ..., 6=sabado, 0=domingo

------------------------------------------------------------------

-- pdias a  devuelve una lista con 12 dias que son los dias de la
--          semana en que comienza cada mes del a�o a siendo
--          1=lunes, 2=martes, ..., 6=sabado y 7=domingo
-- Ejemplo: pdias 2019 es [2,5,5,1,3,6,1,4,7,2,5,7]
pdias :: Int -> [Int]
pdias a = map (pdiasAux a) [1 .. 12]

pdiasAux a 1 = ene1 a
pdiasAux a m
  | mod diasTotales 7 /= 0 = mod diasTotales 7
  | otherwise = 7
  where
    diasTotales = pdiasAux a (m - 1) + diasMes a (m - 1)

diasMes a 2 -- Bisiestos !!
  | mod a 4 == 0  = 29
  | otherwise = 28
diasMes _ m
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31

------------------------------------------------------------------

nombresmeses :: [String]
nombresmeses =
  [ "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  ]

------------------------------------------------------------------

-- fechas pd lm da una lista de 42 dibujos de 1*3 (alguno blanco)
--              con los dias de un mes cuyo primer dia de semana
--              es pd y cuya longitud de mes es lm
agrouparBlancos :: Dibujo -> [Dibujo]
agrouparBlancos d
  | null d = []
  | otherwise = [head d]:agrouparBlancos (tail d)
fechas :: Int -> Int -> [Dibujo]
fechas x y
  | y>31 || x > 7 = error "Error en fechas => datos incoherentes"
  | otherwise =  blancosInicio ++ fechas ++ blancosFinal
    where
      blancosInicio = agrouparBlancos (dibBlanco (3,x-1))
      blancosFinal = agrouparBlancos (dibBlanco (3,43 - x - length fechas))
      fechas = [["  " ++ show a] | a <- [1..y]]
{- Ejemplo:
fechas 3 30
[["   "],["   "],["  1"],["  2"],["  3"],["  4"],["  5"],
 ["  6"],["  7"],["  8"],["  9"],[" 10"],[" 11"],[" 12"],
 [" 13"],[" 14"],[" 15"],[" 16"],[" 17"],[" 18"],[" 19"],
 [" 20"],[" 21"],[" 22"],[" 23"],[" 24"],[" 25"],[" 26"],
 [" 27"],[" 28"],[" 29"],[" 30"],["   "],["   "],["   "],
 ["   "],["   "],["   "],["   "],["   "],["   "],["   "]]
-}

------------------------------------------------------------------

-- otras funciones que se necesiten:

------------------------------------------------------------------
