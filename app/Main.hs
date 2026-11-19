module Main (main) where

import System.IO 
import System.Environment (getArgs)
import Text.Read
import Data.List (delete)

import Motor.Comandos (leerComando)
import Motor.Nucleo 
import Motor.Tipos
import Motor.Reconocedor
import Motor.Persistencia 
import Control.Monad (forM, forM_)
import Data.Char (toLower)
import Motor.Mundo




promt :: Mundo -> Estado -> IO ()
promt mundo estado =
    if verificarMeta mundo estado
    then do
       -- putStrLn (mostrarSala mundo (current estado))
        putStrLn "*** ¡Has ganado! ***"
    else do
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
                    escribirArchivo (arg1 c) txt
                    promt mundo estado
                | cmd c == "cargar" -> do
                    nuevoMundo <- leerArchivo (arg1 c)
                    case leerMundo nuevoMundo of
                        Left errores -> do
                            putStrLn "¡Se encontraron errores en el archivo del mundo!"
                            mapM_ print errores
                            promt mundo estado
                        Right mundoValido -> do
                            let invRecuperado = extraerInventarioGuardado nuevoMundo mundoValido
                            putStrLn "¡Mundo cargado y validado exitosamente!"
                            promt mundoValido (Estado {current = extraerInicio mundoValido, inventario = invRecuperado, capacidad = capMax (cabecera mundoValido)})
                | cmd c == "soltar" -> do
                    let tupla = (soltar mundo (arg1 c) estado)
                    promt (fst tupla) (snd tupla)
                | cmd c == "tomar" -> do
                    let (nuevoMundo, nuevoEstado, mensaje) = tomar mundo (arg1 c) estado
                    putStrLn mensaje
                    promt nuevoMundo nuevoEstado
                | cmd c == "ir" -> do
                    let (nuevoEstado, mensaje) = escogerSalida mundo (current estado) (str2Dir (arg1 c)) estado
                    putStrLn mensaje
                    promt mundo nuevoEstado
                | cmd c == "examinar" -> do
                    putStrLn (mostrarObjeto mundo (arg1 c) 2 estado)
                    promt mundo estado
                | cmd c == "inventario" -> do
                    putStrLn (show (inventario estado))
                    promt mundo estado
                | cmd c == "mirar" -> do
                    putStrLn (mostrarSala mundo (current estado))
                    promt mundo estado
                | cmd c == "ayuda" -> do
                    putStrLn (
                        "ir <direccion>    | Mueve al jugador por esa salida, si existe y no está bloqueada. Las direcciones son norte, sur, este, oeste, arriba y abajo\n" ++
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
                | cmd c == "salir" -> putStrLn ("adiosito")
                | otherwise -> do
                    putStrLn ("comando desconocido")
                    promt mundo estado

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

--captarArgs:: [String] -> Either String String
--captarArgs [] = Left "error: modo de uso: ./aventura <mundo.txt>"
--captarArgs (x:xs) = Right x

data Modo = ModoValidar FilePath | ModoJugar FilePath

captarArgs :: [String] -> Either String Modo
captarArgs ["-validar", ruta] = Right (ModoValidar ruta)
captarArgs [ruta]             = Right (ModoJugar ruta)
captarArgs []                 = Left "Error: modo de uso: aventura mundo.txt"
captarArgs _                  = Left "Error: argumentos inválidos. Uso: aventura mundo.txt"

main :: IO ()
main = do
    args <- getArgs
    case captarArgs args of
        Left fallo -> do
            putStrLn fallo
        Right (ModoValidar ruta) -> do
            contenido <- leerArchivo ruta
            case leerMundo contenido of
                Left errores -> do
                    mapM_ print errores
                Right _ -> do
                    putStrLn "Mundo válido."
        Right (ModoJugar ruta) -> do
            contenido <- leerArchivo ruta
            case leerMundo contenido of
                Left errores -> do
                    putStrLn "¡Se encontraron errores en el archivo del mundo!"
                    mapM_ print errores
                Right mundoValido -> do
                    putStrLn "bienvenido a la aventura"
                    promt mundoValido
                        (Estado
                            { current = extraerInicio mundoValido
                            , inventario = []
                            , capacidad = capMax (cabecera mundoValido)
                            })

--    let mundin = vaciarInfo mundo

--    putStrLn(show (snd mundin))
--    forM_ (lines mundo) $ \linea -> do

--        putStrLn ( imprimir ( head (leerLineaArchivo linea)) )
        
        -- Invocamos leerMundo que valida sintaxis y semántica automáticamente
    
        
