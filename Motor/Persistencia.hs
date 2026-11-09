module Motor.Persistencia () where

import System.IO


leerArchivo :: FilePath -> String
leerArchivo p = do
    s <- readFile p
    s

