module Motor.Mundo (
    leerMundo
) where

import Motor.Tipos
import Motor.Reconocedor
import Data.Char 
import Data.List (nub)
import Control.Monad (forM, forM_)

-- Función principal exigida por el proyecto
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido = 
    let lineasCrudas = lines contenido
        
        -- Primero validamos la sintaxis usando las lineas crudas y el contador recursivo
        erroresSintaxis = validarSintaxis 1 lineasCrudas
    in 
        if not (null erroresSintaxis)
        then Left erroresSintaxis
        else 
            let 
                -- Si la sintaxis es correcta, parseamos de forma global
                lineasParseadas = map sacarLineaP (map leerLineaArchivo lineasCrudas)
                
                mundoConstruido = construirMundo lineasParseadas
                
                -- Validamos la semantica (V2 a V10)
                erroresSemanticos = validarSemantica mundoConstruido
            in
                if not (null erroresSemanticos)
                then Left erroresSemanticos
                else Right mundoConstruido
                
--- Lógica de Construcción ---

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

--convertirCab :: [LineaParseada] -> Cabecera 
--convertirCab (mundo:inicio:meta:xs) = Cabecera (sacarValor mundo) (sacarValor inicio) (sacarValor meta)

convertirCab :: [LineaParseada] -> Cabecera 
convertirCab (mundo:inicio:meta:xs) = 
    Cabecera 
        (sacarValor mundo) 
        (sacarValor inicio) 
        (parsearMeta (sacarValor meta)) 

convertirObjeto :: [LineaParseada] -> [Objeto]
convertirObjeto [] = []
convertirObjeto (obj:desc:xs) = [Objeto (sacarValor obj) (sacarValor desc)] ++ convertirObjeto xs

convertirBloque_Objetos :: [LineaParseada] -> Bloque_Objetos
convertirBloque_Objetos l = Bloque_Objetos (convertirObjeto l)



primeropSalaYDesc:: [LineaParseada] -> ([LineaParseada], String, String)
primeropSalaYDesc [] = ([], [], [])
primeropSalaYDesc (sala:desc:xs) =  (xs, sacarValor sala, sacarValor desc)

-- Conversión de direcciones en minúsculas
stringToDir :: String -> Maybe Direccion
stringToDir s = case map toLower s of
    "norte"  -> Just Norte
    "sur"    -> Just Sur
    "este"   -> Just Este
    "oeste"  -> Just Oeste
    "arriba" -> Just Arriba
    "abajo"  -> Just Abajo
    _        -> Nothing


parsearSalida :: String -> Salida
parsearSalida s = 
    let w = words s
        dirStr = head w
        dir = stringToDir dirStr
        destino = w !! 2
        req = if "requiere" `elem` w
              then Just (last w)
              else Nothing
    in Salida dir destino req

segundoSalidas :: ([LineaParseada], String, String, [Salida]) -> ([LineaParseada], String, String, [Salida] )
segundoSalidas ([], sala, desc, salidas) = ([], sala, desc, salidas)
segundoSalidas ((x:xs), sala, desc,  salidas) 
    | aux x == "SALIDA" = segundoSalidas (xs, sala, desc, salidas ++ [parsearSalida (sacarValor x)])
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

parsearMeta :: String -> Meta
parsearMeta s 
    | "llegar" `elem` w && length w == 3 = LlegarA (last w)
    | "llevar" `elem` w = 
        let objeto = w !! 1
            sala   = last w
        in LlevarA objeto sala
    | otherwise = LlegarA (last w)
  where w = words s

construirMundo :: [LineaParseada] -> Mundo
construirMundo lineasParse = 
    Mundo {
        cabecera = convertirCab (sacarCabecera lineasParse),
        objetos  = convertirBloque_Objetos (sacarObjetos lineasParse),
        salas    = convertirBloque_Salas (proceso1Sala (sacarSalas (miniSacarSalas lineasParse), []))
    }

--- Zona de Validaciones (V1 a V10) usando acumuladores de errores---

-- Funcion auxiliar recursiva que lleva el contador 'n' de la línea
validarSintaxis :: Int -> [String] -> [ErrorMundo]
validarSintaxis _ [] = []
validarSintaxis n (linea:resto) =
    let limpia = trim linea
    in if null limpia || head limpia == '#' || limpia == "---"
       then validarSintaxis (n + 1) resto  -- Ignorar líneas vacías, comentarios o separadores
       else 
           let errorMsg = clasificarErrorSintaxis linea
           in if not (null errorMsg)
              then [ErrorSintaxis n errorMsg] ++ validarSintaxis (n + 1) resto
              else validarSintaxis (n + 1) resto

-- Clasifica el mensaje de error según las reglas V1
clasificarErrorSintaxis :: String -> String
clasificarErrorSintaxis linea
    | not (':' `elem` linea) = "Linea sin la forma CLAVE: valor"
    | otherwise =
        let (claveCruda, valorCrudo) = separarDosPuntos linea
            clave = trim claveCruda
            valor = trim valorCrudo
        in if clave `notElem` ["MUNDO", "INICIO", "META", "OBJETO", "DESC", "SALA", "SALIDA"]
           then "Clave desconocida: " ++ clave
           else if clave `elem` ["SALA", "OBJETO"] && (null valor || any isSpace valor)
                then "Identificador invalido: no debe contener espacios (use guion bajo '_')"
                else if clave == "SALIDA"
                     then 
                         -- Tomamos la primera palabra del valor de la salida
                         let palabras = words valor
                         in if null palabras 
                            then "Salida mal formada: falta la direccion"
                            else 
                                case stringToDir (head palabras) of
                                    Nothing -> "Direccion inexistente o salida mal formada"
                                    Just _  -> ""
                     else ""
                     
separarDosPuntos :: String -> (String, String)
separarDosPuntos [] = ([], [])
separarDosPuntos (c:cs)
    | c == ':'  = ([], cs) -- Encontramos los dos puntos: la clave termina aqui, el resto es el valor
    | otherwise = 
        let (clave, valor) = separarDosPuntos cs
        in (c : clave, valor)
        
        
validarSemantica :: Mundo -> [ErrorMundo]
validarSemantica mundo = 
    -- Aquí aplicamos el concepto de Semigroup/Monoid de las listas ([])
    validarIDsDuplicados mundo
    <> validarReferencias mundo

validarIDsDuplicados :: Mundo -> [ErrorMundo]
validarIDsDuplicados mundo = [] -- Ejemplo: chequear IDs repetidos con 'nub'

validarReferencias :: Mundo -> [ErrorMundo]
validarReferencias mundo = [] -- Ejemplo: chequear que las salidas apunten a salas existentes
