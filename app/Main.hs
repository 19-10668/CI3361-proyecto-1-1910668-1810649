module Main (main) where

import System.IO 
import Text.Read

import Motor.Comandos (leerComando) 
import Motor.Tipos (Comando (Comando, cmd, arg1))

-- ppp :: [(String, String)] -> String
-- ppp [] = []
-- ppp ((x,y):xs) =  x ++ ",  " ++ y ++ ppp xs

-- sss :: [String] -> String
-- sss [] = []
-- sss (x:xs) = x ++ ", " ++ sss xs 

promt :: IO ()
promt = do
    putStr "> "
    hFlush stdout
    linea <- getLine
    case leerComando linea of
        Nothing -> promt
        Just c 
            | cmd c == "salir" -> putStrLn("adiosito")
            |otherwise -> promt
        
            



main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")
    promt



