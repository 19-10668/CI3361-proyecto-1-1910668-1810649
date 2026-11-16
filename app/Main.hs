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
 
instance Show Meta where
    show (LlegarA a) =  a
    show (LlevarA a b) =  a ++ " " ++ b

instance Show Cabecera where
    show (Cabecera a b c) = a ++ " " ++ b ++ " " ++ show c

instance Show Objeto where
    show (Objeto a b) = a ++ b

instance Show Bloque_Objetos where
    show (Bloque_Objetos a) = show a

instance Show Direccion where
    show Norte = "Norte"
    show Sur = "Sur"
    show Este = "Este"
    show Oeste = "Oeste"
    show Arriba = "Arriba"
    show Abajo = "Abajo"

instance Show Salida where
    show (Salida a b c) = show a ++ b ++ show c

instance Show Sala where
    show (Sala a b c d) = a ++ b ++ show c ++ show d

instance Show Bloque_Salas where
    show (Bloque_Salas a) = show a

instance Show Mundo where
    show (Mundo a b c) = show a ++ show b ++ show c

instance Show ErrorMundo where
    show (ErrorSintaxis a b) = (show a) ++ b
    show (ErrorSemantico a) = a 

main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")

    

    -- promt
    

    mundo <- leerArchivo "invalido_semantica.txt"
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
        
