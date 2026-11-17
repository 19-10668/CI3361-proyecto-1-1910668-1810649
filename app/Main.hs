module Main (main) where

import System.IO 
import System.Environment (getArgs)
import Text.Read
import Data.List (delete)

import Motor.Comandos (leerComando) 
import Motor.Tipos
import Motor.Reconocedor
import Motor.Persistencia 
import Control.Monad (forM, forM_)
import Data.Char (toLower)
import Motor.Mundo

-- ppp :: [(String, String)] -> String
-- ppp [] = []
-- ppp ((x,y):xs) =  x ++ ",  " ++ y ++ ppp xs

-- sss :: [String] -> String
-- sss [] = []
-- sss (x:xs) = x ++ ", " ++ sss xs 



buscarSala:: Mundo -> ID_Sala -> Sala
buscarSala mn id = foldl (foldSala) (Sala id "esa sala no existe" [] []) ((\(Bloque_Salas x) -> x) (salas mn))

mostrarSala:: Mundo -> ID_Sala -> String
mostrarSala mn id = 
    let salaActual = buscarSala mn id
    in "estas en " ++ id ++ ": " ++ (\(Sala a b c d)->b)salaActual ++ "\n" ++
    "en la sala hay: " ++ show ((\(Sala a b c d)->d)salaActual) ++ "\n" ++
    "y puedes ir hacia: " ++ show ((\(Sala a b c d)->c)salaActual) ++ "\n"

buscarObjeto::  Mundo -> Int -> Estado -> ID_Objeto -> Objeto
buscarObjeto mn cual es id
    | cual == 1 = foldl (foldObjeto) (Objeto id "No existe tal objeto") ((\(Bloque_Objetos x) -> x) (objetos mn))
    | cual == 2 = foldl (foldObjeto) (Objeto id "No existe tal objeto") ((\(Bloque_Objetos x) -> x) (objetos mn) ++ (inventario es))
    | cual == 3 = foldl (foldObjeto) (Objeto id "No existe tal objeto") ((inventario es))



mostrarObjeto:: Mundo -> ID_Objeto -> Int -> Estado -> String
mostrarObjeto mn id cual es = (\(Objeto id d)-> d) (buscarObjeto mn cual es id )


buscarSalida:: Sala -> Direccion -> ID_Sala
buscarSalida mn dir = salaDestino (foldl (foldSalida) (Salida {dirSalida = Just dir, salaDestino = "", reqObjeto = Just ""}) ((\(Sala a b c d) ->c)mn))

escogerSalida :: Mundo -> ID_Sala -> Direccion -> Estado -> Estado
escogerSalida mn sala dir es = Estado {current = (buscarSalida (buscarSala mn sala) dir), inventario = inventario es, capacidad = capacidad es}

str2Dir:: String -> Direccion
str2Dir s 
    | s == "norte" = Norte
    | s == "sur" = Sur
    | s == "este" = Este
    | s == "oeste" = Oeste
    | s == "arriba" = Arriba
    | s == "abajo" = Abajo

tomar:: Mundo -> ID_Objeto -> Estado ->(Mundo, Estado)
tomar mn id es = (Mundo {
    cabecera = cabecera mn,
    objetos = objetos mn,
    salas = (tomarPoner (salas mn) mn (current es) es id)}, es {inventario = (inventario es) ++ [buscarObjeto mn 1 es id]})

tomarPoner:: Bloque_Salas -> Mundo -> ID_Sala -> Estado -> ID_Objeto -> Bloque_Salas
tomarPoner (Bloque_Salas salas) mn id es ido=
    let old = buscarSala mn id
        old_ID_Sala = (\(Sala a b c d) -> a) old
        old_Desc = (\(Sala a b c d) -> b) old
        old_Salidas = (\(Sala a b c d) -> c) old
        old_objetos = (\(Sala a b c d) -> d) old
        new_Sala = Sala old_ID_Sala old_Desc old_Salidas ( delete ( (\(Objeto n d) -> n) (buscarObjeto mn 1 es ido))  ((\(Sala a b c d)->d) (buscarSala mn (current es))))
    in Bloque_Salas ( (delete old salas) ++ [new_Sala] )

soltar:: Mundo -> ID_Objeto -> Estado ->(Mundo, Estado)
soltar mn id es = (Mundo {
    cabecera = cabecera mn,
    objetos = objetos mn,
    salas = (soltarPoner (salas mn) mn (current es) es id)}, es {inventario = (delete (buscarObjeto mn 3 es id) (inventario es))})



soltarPoner:: Bloque_Salas -> Mundo -> ID_Sala -> Estado -> ID_Objeto -> Bloque_Salas
soltarPoner (Bloque_Salas salas) mn id es ido=
    let old = buscarSala mn id
        old_ID_Sala = (\(Sala a b c d) -> a) old
        old_Desc = (\(Sala a b c d) -> b) old
        old_Salidas = (\(Sala a b c d) -> c) old
        old_objetos = (\(Sala a b c d) -> d) old
        new_Sala = Sala old_ID_Sala old_Desc old_Salidas (old_objetos ++ [((\(Objeto n d) -> n) (buscarObjeto mn 3 es ido))])
    in Bloque_Salas ( (delete old salas) ++ [new_Sala] )


promt ::Mundo -> Estado-> IO ()
promt mundo estado= do
    putStr "> "
    hFlush stdout
    linea <- getLine
    case leerComando linea of
        Nothing -> do
                putStrLn ("comando desconocido")
                promt mundo estado
        Just c 
            | cmd c == "guardar" -> do
                let txt = pasarMundo mundo estado 
                -- putStrLn( txt )
                escribirArchivo (arg1 c) txt
                promt mundo estado
            | cmd c == "cargar" -> do
                nuevoMundo <- leerArchivo (arg1 c)
                case leerMundo nuevoMundo of
                    Left errores -> do
                        putStrLn "¡Se encontraron errores en el archivo del mundo!"
                        -- Imprimimos todos los errores acumulados (V1 a V10)
                        mapM_ print errores
                        promt mundo estado
                        
                    Right mundoValido -> do
                        putStrLn "¡Mundo cargado y validado exitosamente!"
                        promt mundoValido (Estado {current = (extraerInicio mundoValido), inventario = [], capacidad = 5})
                        -- print mundoValido
            | cmd c == "soltar" -> do
                let tupla = (soltar mundo (arg1 c) estado)
                promt (fst tupla) (snd tupla)
            | cmd c == "tomar" -> do
                let tupla = (tomar mundo (arg1 c) estado) 
                -- putStrLn (show ( delete ( (\(Objeto n d) -> n) (buscarObjeto mundo 1 estado (arg1 c)))  ((\(Sala a b c d)->d) (buscarSala mundo (current estado)))))
                -- putStrLn (show (fst tupla))
                promt (fst tupla) (snd tupla) -- no va a fallr porque si no estaba mala la funcion tomar
            | cmd c == "ir" -> do
                promt mundo ((escogerSalida mundo (current estado) (str2Dir (arg1 c)) estado))
            | cmd c == "examinar" -> do
                putStrLn (mostrarObjeto mundo (arg1 c) 2 estado)
                promt mundo estado
            | cmd c == "inventario" -> do
                putStrLn(show (inventario estado))
                promt mundo estado
            | cmd c == "mirar" -> do
                putStrLn( mostrarSala mundo (current estado))
                promt mundo estado
            | cmd c == "ayuda" -> do
                putStrLn(
                    "ir <direccion>    | Mueve al jugador por esa salida, si existe y no está bloqueada. Las direcciones son norte, sur, este, oeste, arriba y abajo\n"++
                    "mirar             | Describe la sala actual: su descripción, los objetos que hay y las salidas.\n" ++
                    "tomar <objeto>    | Pasa un objeto de la sala al inventario. Sinónimo: coger.\n" ++
                    "soltar <objeto>   | Deja un objeto del inventario en la sala actual.\n" ++
                    "examinar <objeto> | Muestra la descripción de un objeto del inventario o de la sala.\n" ++
                    "inventario        | Lista lo que lleva el jugador. Sinónimo: inv.\n" ++
                    "guardar <archivo> | Guarda la partida en un archivo.\n" ++
                    "cargar <archivo>  | Recupera una partida guardada.\n" ++
                    "ayuda             | Muestra los comandos disponibles.\n" ++
                    "salir             | Termina el programa.\n"
                    )
                promt mundo estado
            
            | cmd c == "salir" -> putStrLn("adiosito")
            |otherwise -> do
                putStrLn ("comando desconocido")
                promt mundo estado


foldSala:: Sala -> Sala -> Sala
foldSala b a  
    | (\(Sala id n o m)-> id) b == (\(Sala id n o m)-> id) a = a
    | otherwise = b

foldObjeto:: Objeto -> Objeto -> Objeto
foldObjeto b a 
    | (\(Objeto id d)-> id) b == (\(Objeto id d)-> id) a = a
    | otherwise = b

foldSalida:: Salida -> Salida -> Salida 
foldSalida b a 
    | (\(Salida dir sala obj)-> dir) b == (\(Salida dir sala obj)-> dir) a = a
    | otherwise = b


extraerCabecera:: Mundo -> Cabecera
extraerCabecera m = cabecera m

extraerInicio:: Mundo -> ID_Sala
extraerInicio m = salaInicio (extraerCabecera m)


pasarMundo:: Mundo -> Estado -> String
pasarMundo mn es = 
    let titulo = "MUNDO: " ++ tituloMundo (cabecera mn) ++ "\n"
        inicio = "INICIO: " ++ salaInicio (cabecera mn) ++ "\n"
        meta = pasarMeta (metaMundo (cabecera mn))
        totalObjetos = (\(a,b) -> b) (pasarObjetos( ((\(Bloque_Objetos x) -> x)(objetos mn)), ""))
        totalSalas = (\(a,b) -> b) (pasarSalas (((\(Bloque_Salas x) -> x)(salas mn)), ""))
    in titulo ++ inicio ++ meta ++ totalObjetos ++ totalSalas
 
pasarObjetos:: ([Objeto], String) -> ([Objeto], String)
pasarObjetos ([], acc) = ([], acc)
pasarObjetos ((x:xs), acc) = pasarObjetos (xs, acc ++ "---\n" ++ "OBJETO: " ++ ((\(Objeto o d)-> o)x) ++ "\n" ++ "DESC: " ++ ((\(Objeto o d)-> d)x) ++ "\n") 

pasarSalas:: ([Sala], String) -> ([Sala], String)
pasarSalas ([], acc) = ([], acc)
pasarSalas ((x:xs), acc) = pasarSalas (xs, acc ++ "---\n" ++ "SALA: " ++ (\(Sala a b c d) -> a) x ++ "\n"
    ++ "DESC: " ++ (\(Sala a b c d) -> b) x ++ "\n"
    ++ (\(a, b) -> b) (pasarSalidas ((\(Sala a b c d) -> c) x, ""))
    ++ (\(a, b) -> b) (pasarNombreObjeto ((\(Sala a b c d) -> d) x, "")))

pasarSalidas:: ([Salida], String) -> ([Salida], String)
pasarSalidas ([], acc) = ([], acc)
pasarSalidas ((x:xs), acc) 
    | reqObjeto x == Nothing = pasarSalidas (xs, acc ++ "SALIDA: " ++ (formatearDir (dirSalida x)) ++ " -> " ++ (salaDestino x) ++ "\n")
    | reqObjeto x /= Nothing = pasarSalidas (xs, acc ++ "SALIDA: " ++ (formatearDir (dirSalida x)) ++ " -> " ++ (salaDestino x) ++ " requiere " ++ (\(Just z)->z)(reqObjeto x) ++ "\n") 

pasarNombreObjeto :: ([ID_Objeto], String) -> ([ID_Objeto], String)
pasarNombreObjeto ([], acc) = ([], acc)
pasarNombreObjeto ((x:xs), acc) = pasarNombreObjeto (xs, acc ++ "OBJETO: " ++ x ++ "\n")

formatearDir:: Maybe Direccion -> String
formatearDir (Just x)
    | x == Norte = "norte"
    | x == Sur = "sur"
    | x == Este = "este"
    | x == Oeste = "oeste"
    | x == Arriba = "arriba"
    | x == Abajo = "abajo"



pasarMeta :: Meta -> String
pasarMeta (LlegarA sala) = "META: llegar a " ++ sala ++ "\n"
pasarMeta (LlevarA sala obj) = "META: llevar " ++ obj ++ " a " ++ sala ++ "\n"


-- instance Show Meta where
-- instance Show Objeto where
--     show (Objeto a b) = a ++ b

-- instance Show Bloque_Objetos where
--     show (Bloque_Objetos a) = show a

-- instance Show Direccion where
--     show Norte = "Norte"
--     show Sur = "Sur"
--     show Este = "Este"
--     show Oeste = "Oeste"
--     show Arriba = "Arriba"
--     show Abajo = "Abajo"

-- instance Show Salida where
--     show (Salida a b c) = show a ++ b ++ show c

-- instance Show Sala where
--     show (Sala a b c d) = a ++ b ++ show c ++ show d

-- instance Show Bloque_Salas where
--     show (Bloque_Salas a) = show a

-- instance Show Mundo where
--     show (Mundo a b c) = show a ++ show b ++ show c

-- instance Show ErrorMundo where
--     show (ErrorSintaxis a b) = (show a) ++ b
--     show (ErrorSemantico a) = a 

captarArgs:: [String] -> Either String String
captarArgs [] = Left "error: modo de uso: ./aventura <mundo.txt>"
captarArgs (x:xs) = Right x

main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")

    args <- getArgs
    
    -- promt
    
    case captarArgs args of
        Left fallo -> do
            putStrLn fallo
        Right arg -> do
            mundo <- leerArchivo (arg)
            case leerMundo mundo of
                Left errores -> do
                    putStrLn "¡Se encontraron errores en el archivo del mundo!"
                    -- Imprimimos todos los errores acumulados (V1 a V10)
                    mapM_ print errores
                    
                Right mundoValido -> do
                    putStrLn "¡Mundo cargado y validado exitosamente!"
                    -- putStrLn (pasarMundo mundoValido (Estado {current = (extraerInicio mundoValido), inventario = [], capacidad = 5}))
                    promt mundoValido (Estado {current = (extraerInicio mundoValido), inventario = [], capacidad = 5})
                    -- print mundoValido





--    let mundin = vaciarInfo mundo

--    putStrLn(show (snd mundin))
--    forM_ (lines mundo) $ \linea -> do

--        putStrLn ( imprimir ( head (leerLineaArchivo linea)) )
        
        -- Invocamos leerMundo que valida sintaxis y semántica automáticamente
    
        
