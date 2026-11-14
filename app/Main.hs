module Main (main) where

import System.IO 
import Text.Read

import Motor.Comandos (leerComando) 
import Motor.Tipos
import Motor.Reconocedor
import Motor.Persistencia 
import Control.Monad (forM, forM_)
--import Text.Read (readMaybe)
import Data.Char (toLower)
import Motor.Mundo

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


--vaciarInfo :: String -> (Bloque_Salas, Mundo)
--vaciarInfo mundin = 
  --  let lineasParse = map sacarLineaP (map (leerLineaArchivo) (lines mundin)) -- :: [ [(LineaParseada, String)] ] -> [LineaParseada]
    
    
    
 --   in (  convertirBloque_Salas ( proceso1Sala ( (sacarSalas (miniSacarSalas lineasParse), []) )), 
   -- Mundo {
    --cabecera = convertirCab (sacarCabecera lineasParse),
  --  objetos  = convertirBloque_Objetos (sacarObjetos lineasParse),
   -- salas = convertirBloque_Salas ( proceso1Sala ( (sacarSalas (miniSacarSalas lineasParse), []) ))-
--})

-- mostrameElMundo :: Mundo -> String
-- mostrameElMundo m = 

--data Coso = Coso {
 --   cs :: Int,
 --   gh :: String
--} deriving (Show, Eq)
--data Mini = Mini Int String

--func :: String -> Mini
--func s = Mini 
 --   (
 --       4
 --   ) 
 --   "" 
 

main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")

    

    -- promt
    

    mundo <- leerArchivo "invalido_sintaxis.txt"
--    let mundin = vaciarInfo mundo

--    putStrLn(show (snd mundin))
--    forM_ (lines mundo) $ \linea -> do

--        putStrLn ( imprimir ( head (leerLineaArchivo linea)) )
        
        -- Invocamos leerMundo que valida sintaxis y semántica automáticamente
    case leerMundo mundo of
        Left errores -> do
            putStrLn "¡Se encontraron errores en el archivo del mundo!"
            -- Imprimimos todos los errores acumulados (V1 a V10)
            mapM_ print errores
            
        Right mundoValido -> do
            putStrLn "¡Mundo cargado y validado exitosamente!"
            -- Aquí puedes mostrar los datos del mundo o iniciar el juego con el prompt
            print mundoValido
        
