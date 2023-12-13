-----------------------------------------------------------------
-- PRACTICA CALENDARIO INTERACTIVO     2023-24
-----------------------------------------------------------------

-- La llamada principal debe ser main

module CalendarioInteractivo where

import Calendarios (printCalendario)           -- importar el modulo de la Practica 1
import Data.Char (isDigit)

-----------------------------------------------------------------
---------------------LISTAS AUXILIARES---------------------------
-- COMPRUEBA QUE TODO EL CONTENIDO DE UN STRING SEAN NUMEROS
esNumero :: [Char] -> Bool
esNumero [] = True
esNumero entrada
    | isDigit (head entrada) = esNumero (tail entrada)
    | otherwise = False

-- COMPRUEBA QUE TODO EL CONTENIDO DEL ANO CUMPLA CON LAS CONDICIONES DEL ENUNCIADO
esAnio :: [Char] -> Bool
esAnio entrada
    | entrada == "" = False
    | not (esNumero entrada) || num < 1 || num > 9999 = False
    | otherwise = True
    where num = read entrada :: Int

-- COMPRUEBA SI EL STRING DE LA ENTRADA ES UN NUMERO 3 O 4
numer3o4 :: [Char] -> Bool
numer3o4 entrada =  n == 3 || n == 4
    where n = read entrada :: Int

-------------------------------------------------------------------
---------------------METODOS PRINCIPALES---------------------------
preguntarFecha :: IO Int
preguntarFecha = do
    anio <- getLine
    if not (esAnio anio)
        then do
            putStr "--- Dime una fecha correcta no mayor de 4 digitos: "
            preguntarFecha
    else return (read anio :: Int)

preguntarColumnas :: IO Int
preguntarColumnas = do
    col <- getLine
    if not (esNumero col && numer3o4 col) then do
            putStr "--- El numero de columnas debe ser un numero entre 3 y 4: "
            preguntarColumnas
    else return (read col :: Int)

main :: IO ()
main = do
    putStrLn "PRACTICA CALENDARIO INTERACTIVO     2023-24"
    putStr "- Dime que anio quieres visualizar: "
    anio <- preguntarFecha
    putStr "- Dime en cuantas columnas quieres verlo: "
    col <- preguntarColumnas
    printCalendario col anio 