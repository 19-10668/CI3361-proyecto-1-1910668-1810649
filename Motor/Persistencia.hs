module Motor.Persistencia () where

import System.IO
import Motor.Reconocedor


leerArchivo :: FilePath -> IO String
leerArchivo p = do
    s <- readFile p
    s

