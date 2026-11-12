module Main (main) where

import System.IO 
import Text.Read

import Motor.Comandos (leerComando) 
import Motor.Tipos
import Motor.Reconocedor
import Motor.Persistencia 
import Control.Monad (forM, forM_)

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
        
            

type ID_Sala = String
type ID_Objeto = String
type Desc = String


data Cabecera = Cabecera String ID_Sala String deriving (Show, Eq)

data Objeto = Objeto ID_Objeto Desc deriving (Show, Eq)
data Bloque_Objetos = Bloque_Objetos [Objeto] deriving (Show, Eq)


data Sala = Sala ID_Sala Desc [Salida] [ID_Objeto] deriving (Show, Eq)
data Salida = Salida String deriving (Show, Eq)
-- data Salida = Salida_libre String ID_Sala | Salida_compleja String ID_Sala ID_Objeto deriving (Show, Eq, Read)
data Bloque_Salas = Bloque_Salas [Sala] deriving (Show, Eq)

data Mundo = Mundo{
    cabecera :: Cabecera,
    objetos :: Bloque_Objetos,
    salas :: Bloque_Salas
} deriving (Show, Eq)




aux :: LineaParseada -> String
aux (LClave k v) = k
aux (LSeparador) = "separador"
aux (LVacia) = "vacia"

sacarValor :: LineaParseada -> String
sacarValor (LClave k v) = v

sacarCabecera :: [LineaParseada] -> [LineaParseada] -- esto tiene que tenber ecactamente 3 valores
sacarCabecera [] = []
sacarCabecera (x:xs) 
    | aux x == "MUNDO" = [x] ++ sacarCabecera xs
    | aux x == "INICIO" = [x] ++ sacarCabecera xs
    | aux x == "META" = [x] ++ sacarCabecera xs
    | aux x == "separador" || aux x == "vacia" = [] ++ sacarCabecera xs
    | otherwise = []


sacarObjetos :: [LineaParseada] -> [LineaParseada]
sacarObjetos [] = []
sacarObjetos (x:xs)
    | aux x == "OBJETO" = [x] ++ sacarObjetos xs
    | aux x == "DESC" = [x] ++ sacarObjetos xs
    | aux x == "separador" || aux x == "vacia" || aux x == "MUNDO" || aux x == "INICIO" || aux x == "META" = [] ++ sacarObjetos xs
    | otherwise = []

miniSacarSalas :: [LineaParseada] -> [LineaParseada]
miniSacarSalas (x:xs)
    | aux x == "SALA" = [x] ++ xs 
    | otherwise = miniSacarSalas xs

sacarSalas :: [LineaParseada] -> [LineaParseada]
sacarSalas [] = []
sacarSalas (x:xs)
    | aux x == "SALA" = [x] ++ sacarSalas xs
    | aux x == "DESC" = [x] ++ sacarSalas xs
    | aux x == "SALIDA" = [x] ++ sacarSalas xs
    | aux x == "OBJETO" = [x] ++ sacarSalas xs
    | aux x == "separador" || aux x == "vacia" = [] ++ sacarSalas xs
    | otherwise = []

convertirCab :: [LineaParseada] -> Cabecera 
convertirCab (mundo:inicio:meta:xs) = Cabecera (sacarValor mundo) (sacarValor inicio) (sacarValor meta)

convertirObjeto :: [LineaParseada] -> [Objeto]
convertirObjeto [] = []
convertirObjeto (obj:desc:xs) = [Objeto (sacarValor obj) (sacarValor desc)] ++ convertirObjeto xs

convertirBloque_Objetos :: [LineaParseada] -> Bloque_Objetos
convertirBloque_Objetos l = Bloque_Objetos (convertirObjeto l)



primeropSalaYDesc:: [LineaParseada] -> ([LineaParseada], String, String)
primeropSalaYDesc [] = ([], [], [])
primeropSalaYDesc (sala:desc:xs) =  (xs, sacarValor sala, sacarValor desc)

segundoSalidas :: ([LineaParseada], String, String, [Salida]) -> ([LineaParseada], String, String, [Salida] )
segundoSalidas ([], sala, desc, salidas) = ([], sala, desc, salidas)
segundoSalidas ((x:xs), sala, desc,  salidas) 
    | aux x == "SALIDA" = segundoSalidas (xs, sala, desc, salidas ++ [Salida (sacarValor x)])
    | otherwise = ([x] ++ xs, sala, desc, salidas)

terceroObjetos :: ([LineaParseada], String, String, [Salida], [ID_Objeto]) -> ([LineaParseada], String, String, [Salida], [ID_Objeto] )
terceroObjetos ([], sala, desc, salidas,  obj) = ([], sala, desc, salidas,  obj) 
terceroObjetos ((x:xs), sala, desc, salidas,  obj)
    | aux x == "OBJETO" = terceroObjetos (xs, sala, desc, salidas, obj ++ [sacarValor x])
    | otherwise = ([x] ++ xs, sala, desc, salidas, obj)


proceso1Sala :: ([LineaParseada], [Sala]) -> ([LineaParseada], [Sala])
proceso1Sala ([], completa) = ([], completa)
proceso1Sala ((z:zs), completa) 
    | aux z == "SALA" = proceso1Sala ((\(lista, c) -> (lista, completa ++ [c])  ) (proceso2Sala (terceroObjetos ((\(xs, sala, desc, salidas) -> (xs, sala, desc, salidas, [])) (segundoSalidas ((\(xs, sala, desc) -> (xs, sala, desc, [])) (primeropSalaYDesc ([z] ++ zs) )))))))
    
    
convertirBloque_Salas :: ([LineaParseada], [Sala]) -> Bloque_Salas
convertirBloque_Salas (l, salas) = Bloque_Salas salas

proceso2Sala :: ([LineaParseada], String, String, [Salida], [ID_Objeto]) -> ([LineaParseada], Sala)
proceso2Sala (xs, sala, desc, salidas, obj) = (xs, Sala sala desc salidas obj)





vaciarInfo :: String -> (Bloque_Salas, Mundo)
vaciarInfo mundin = 
    let lineasParse = map sacarLineaP (map (leerLineaArchivo) (lines mundin)) -- :: [ [(LineaParseada, String)] ] -> [LineaParseada]
    
    
    
    in (  convertirBloque_Salas ( proceso1Sala ( (sacarSalas (miniSacarSalas lineasParse), []) )), 
    Mundo {
    cabecera = convertirCab (sacarCabecera lineasParse),
    objetos  = convertirBloque_Objetos (sacarObjetos lineasParse),
    salas = convertirBloque_Salas ( proceso1Sala ( (sacarSalas (miniSacarSalas lineasParse), []) ))
})

-- mostrameElMundo :: Mundo -> String
-- mostrameElMundo m = 

data Coso = Coso {
    cs :: Int,
    gh :: String
} deriving (Show, Eq)
data Mini = Mini Int String

func :: String -> Mini
func s = Mini 
    (
        4
    ) 
    "" 
 

main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")

    

    -- promt
    

    mundo <- leerArchivo "entrega.txt"
    let mundin = vaciarInfo mundo

    putStrLn(show (snd mundin))
    forM_ (lines mundo) $ \linea -> do

        putStrLn ( imprimir ( head (leerLineaArchivo linea)) )
        
