module Motor.Persistencia (leerArchivo, escribirArchivo) where

import System.IO
import System.Directory (doesFileExist)
import Motor.Reconocedor


leerArchivo :: FilePath -> IO (Maybe String)
leerArchivo p = do
    existe <- doesFileExist p
    if not existe
        then return Nothing
        else do
            contenido <- readFile p
            -- lo siguiente sirve para forzar a Haskell a leer todo el archivo de inmediato y cerrarlo, en lugar de dejarlo abierto esperando a que se necesite el texto
            length contenido `seq` return (Just contenido)


escribirArchivo :: FilePath -> String -> IO ()
escribirArchivo p s = writeFile p s
