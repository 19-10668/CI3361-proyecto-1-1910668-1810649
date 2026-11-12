module Motor.Persistencia (leerArchivo) where

import System.IO
import Motor.Reconocedor


leerArchivo :: FilePath -> IO String
leerArchivo p = readFile p


