module Motor.Persistencia (leerArchivo, escribirArchivo) where

import System.IO
import Motor.Reconocedor


leerArchivo :: FilePath -> IO String
leerArchivo p = readFile p


escribirArchivo :: FilePath -> String -> IO ()
escribirArchivo p s = writeFile p s