module Motor.Mundo (
    leerMundo
) where

import Motor.Tipos
import Motor.Reconocedor
import Data.Char 
import Data.List 
import Control.Monad (forM, forM_)
import System.IO.Unsafe

-- Función principal exigida por el proyecto
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido = 
    let lineasCrudas = lines contenido
        
        -- Primero validamos la sintaxis usando las lineas crudas y el contador recursivo
        erroresSintaxis = validarSintaxis 1 [] lineasCrudas
    in 
        if not (null erroresSintaxis)
        then Left erroresSintaxis
        else 
            let 
                -- Si la sintaxis es correcta, parseamos de forma global
                lineasParseadas = map sacarLineaP (map leerLineaArchivo lineasCrudas)
                
                mundoConstruido = construirMundo lineasParseadas
                -- Extraemos los IDs de los objetos que están guardados en el inventario del archivo
                invEnArchivo = concat [words v | LClave "INVENTARIO" v <- lineasParseadas, map toLower v /= "vacio"]
                
                -- Validamos la semantica (V2 a V10)
                erroresSemanticos = validarSemantica mundoConstruido invEnArchivo
            in
                if not (null erroresSemanticos)
                then Left erroresSemanticos
                else Right mundoConstruido
     
{-- Función modificada para saltarse las validaciones e inspeccionar el parser directamente
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido =  
    let lineasCrudas = lines contenido
        -- Opcional: puedes dejar la sintaxis básica, o parsear directo
        lineasParseadas = map sacarLineaP (map leerLineaArchivo lineasCrudas)
        
        -- Construimos el mundo directamente sin importar si hay errores semánticos o de V9
        mundoConstruido = construirMundo lineasParseadas
    in 
        Right mundoConstruido --  Forzamos que siempre devuelva el mundo parseado con éxito
--- Lógica de Construcción-}

aux :: LineaParseada -> String
aux (LClave k v) = k
aux (LSeparador) = "separador"
aux (LVacia) = "vacia"

sacarValor :: LineaParseada -> String
sacarValor (LClave k v) = v

sacarCabecera :: [LineaParseada] -> [LineaParseada] 
sacarCabecera [] = []
sacarCabecera (x:xs) 
    | aux x == "MUNDO" = [x] ++ sacarCabecera xs
    | aux x == "INICIO" = [x] ++ sacarCabecera xs
    | aux x == "META" = [x] ++ sacarCabecera xs
    | aux x == "CAPACIDAD" = [x] ++ sacarCabecera xs
    | aux x == "INVENTARIO" = sacarCabecera xs -- Ignoramos la linea de inventario en la cabecera del mundo
    | aux x == "separador" || aux x == "vacia" = [] ++ sacarCabecera xs
    | otherwise = []


sacarObjetos :: [LineaParseada] -> [LineaParseada]
sacarObjetos [] = []
sacarObjetos (x:xs)
    | aux x == "OBJETO" = x : sacarObjetos xs
    | aux x == "DESC"   = x : sacarObjetos xs
    | aux x == "PESO"   = x : sacarObjetos xs
    | aux x `elem` ["separador", "vacia", "MUNDO", "INICIO", "META", "CAPACIDAD", "INVENTARIO"] = sacarObjetos xs
    | otherwise = [] -- Se detiene al llegar al primer "SALA"
    
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


{-convertirCab :: [LineaParseada] -> Cabecera 
convertirCab (mundo:inicio:meta:capacidadLine:xs) = 
    Cabecera 
        (sacarValor mundo) 
        (sacarValor inicio) 
        (parsearMeta (sacarValor meta))
        (read (sacarValor capacidadLine))-}

-- esto es por si el archivo no trae capacidad
convertirCab :: [LineaParseada] -> Cabecera 
convertirCab lineas = 
    let mundoVal  = case [v | LClave "MUNDO" v <- lineas] of (x:_) -> x; [] -> ""
        inicioVal = case [v | LClave "INICIO" v <- lineas] of (x:_) -> x; [] -> ""
        metaVal   = case [v | LClave "META" v <- lineas] of (x:_) -> parsearMeta x; [] -> LlegarA ""
        capVal    = case [v | LClave "CAPACIDAD" v <- lineas] of 
                      (x:_) -> case reads x of [(n, "")] -> n; _ -> 10
                      []    -> 10 -- Capacidad por defecto si el archivo no la trae
    in Cabecera mundoVal inicioVal metaVal capVal

--convertirObjeto :: [LineaParseada] -> [Objeto]
--convertirObjeto [] = []
--convertirObjeto (obj:desc:xs) = [Objeto (sacarValor obj) --(sacarValor desc)] ++ convertirObjeto xs

convertirObjeto :: [LineaParseada] -> [Objeto]
convertirObjeto [] = []
convertirObjeto (LClave "OBJETO" idObj : LClave "DESC" d : LClave "PESO" pStr : xs) =
    let p = case reads (trim pStr) of
                [(n, "")] -> n
                _         -> 1
    in Objeto idObj d p : convertirObjeto xs
convertirObjeto (LClave "OBJETO" idObj : LClave "DESC" d : xs) =
    Objeto idObj d 1 : convertirObjeto xs
convertirObjeto (_:xs) = convertirObjeto xs

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
    -- aca tampoco va a fallar head pues la salida que estamos parseando es una cosa que existe ya
    -- asi que words da aunque sea algo (la direccion)
        dirStr = head w
        dir = stringToDir dirStr
        destino = trim (w !! 2)
        req = if  elem "requiere" w
              then Just (trim (last w))
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
    | elem "llegar" w && length w == 3 = LlegarA (last w)
    | elem "llevar" w = 
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

--- Zona de Validaciones (V1 a V10) usando acumuladores de errores

-- Lista de claves de cabecera obligatorias
clavesCabeceraObligatorias :: [String]
clavesCabeceraObligatorias = ["MUNDO", "INICIO", "META", "CAPACIDAD"]

-- Funcion auxiliar recursiva que lleva el contador 'n' de la línea
validarSintaxis :: Int -> [String] -> [String] -> [ErrorMundo]
validarSintaxis _ vistas [] = 
    -- Al terminar de leer todas las lineas, verificamos si falto alguna clave obligatoria
    [ ErrorSintaxis 1 ("Falta la clave obligatoria de cabecera: " ++ k) 
    | k <- clavesCabeceraObligatorias, notElem k vistas ]
validarSintaxis n vistas (linea:resto) =
    let limpia = trim linea
    -- head aqui no falla. la razon es el corto circuito
    in if null limpia || head limpia == '#' || limpia == "---"
       then validarSintaxis (n + 1) vistas resto -- Ignorar líneas vacías, comentarios o separadores
       else 
           let (errorMsg, nuevasVistas) = clasificarErrorSintaxis linea vistas
           in if not (null errorMsg)
              then ErrorSintaxis n errorMsg : validarSintaxis (n + 1) nuevasVistas resto
              else validarSintaxis (n + 1) nuevasVistas resto
                     
clasificarErrorSintaxis :: String -> [String] -> (String, [String])
clasificarErrorSintaxis linea vistas
    | not (elem ':' linea) = ("Linea sin la forma CLAVE: valor", vistas)
    | otherwise =
        let (claveCruda, valorCrudo) = separarDosPuntos linea
            clave = trim claveCruda
            valor = trim valorCrudo
            esCabecera = elem clave clavesCabeceraObligatorias
            nuevasVistas = if esCabecera && notElem clave vistas 
                           then clave : vistas 
                           else vistas
        in if notElem clave ["MUNDO", "INICIO", "META", "OBJETO", "DESC", "SALA", "SALIDA", "CAPACIDAD", "PESO", "INVENTARIO"]
           then ("Clave desconocida: " ++ clave, vistas)
           else if esCabecera && elem clave vistas
                then ("Clave de cabecera repetida: " ++ clave, vistas)
                else if null valor
                     then ("La clave '" ++ clave ++ "' no tiene valor", nuevasVistas)
                     else if elem clave ["SALA", "OBJETO"] && any isSpace valor
                          then ("Identificador invalido: no debe contener espacios (use guion bajo '_')", nuevasVistas)
                          else if clave == "CAPACIDAD" || clave == "PESO"
                               then if not (all isDigit valor) || read valor <= 0
                                    then ("Valor invalido: debe ser un entero positivo", nuevasVistas)
                                    else ("", nuevasVistas)
                               else if clave == "META"
                                    then 
                                        let palabras = words valor
                                        -- head aqui tampoco falla, pues ya verificamos que palabras no sea null 
                                        in if null palabras || 
                                              (head palabras == "llegar" && length palabras /= 3) || 
                                              (head palabras == "llevar" && length palabras /= 4) ||
                                              notElem (head palabras) ["llegar", "llevar"]
                                           then ("Meta mal formada. Use: 'META: llegar a [sala]' o 'META: llevar [objeto] a [sala]'", nuevasVistas)
                                           else ("", nuevasVistas)
                                    else if clave == "SALIDA"
                                         then
                                             let palabras = words valor
                                             in if null palabras 
                                                then ("Salida mal formada: falta la direccion", nuevasVistas)
                                                else 
                                                    case stringToDir (head palabras) of
                                                        Nothing -> ("Direccion inexistente o salida mal formada", nuevasVistas)
                                                        Just _  -> ("", nuevasVistas)
                                         else ("", nuevasVistas)
                
separarDosPuntos :: String -> (String, String)
separarDosPuntos [] = ([], [])
separarDosPuntos (c:cs)
    | c == ':'  = ([], cs) -- Encontramos los dos puntos: la clave termina aqui, el resto es el valor
    | otherwise = 
        let (clave, valor) = separarDosPuntos cs
        in (c : clave, valor)
        
-- usamos el monoide de las listas [] para acumular y encadenar todos los errores.       
validarSemantica :: Mundo -> [String] -> [ErrorMundo]
validarSemantica mundo invArchivo = 
    validarIDsDuplicados mundo <> validarReferencias mundo invArchivo

-- V2: IDs duplicados (Salas u Objetos)

validarIDsDuplicados :: Mundo -> [ErrorMundo]
validarIDsDuplicados mundo = 
    let (Bloque_Objetos listaObjs) = objetos mundo
        (Bloque_Salas listaSalas) = salas mundo
        
        idsObjs = [oID | Objeto oID _ _<- listaObjs]
        idsSalas = [sID | Sala sID _ _ _ <- listaSalas]
        
        duplicadosObjs = encontrarDuplicados idsObjs
        duplicadosSalas = encontrarDuplicados idsSalas
        
        errsObjs = [ErrorSemantico ("Error V2: Objeto duplicado: " ++ o) | o <- duplicadosObjs]
        errsSalas = [ErrorSemantico ("Error V2: Sala duplicada: " ++ s) | s <- duplicadosSalas]
    in errsObjs ++ errsSalas

-- V3 a V10

validarReferencias :: Mundo -> [String] -> [ErrorMundo]
validarReferencias mundo  invArchivo = 
    let (Bloque_Objetos listaObjs) = objetos mundo
        (Bloque_Salas listaSalas) = salas mundo
        maxCap = capMax (cabecera mundo)
        
        setObjs = [oID | Objeto oID _ _ <- listaObjs]
        setSalas = [sID | Sala sID _ _ _ <- listaSalas]
        
        todasSalidas = [(sID, sal) | Sala sID _ sals _ <- listaSalas, sal <- sals]
        todosObjsEnSalas = [oID | Sala _ _ _ objs <- listaSalas, oID <- objs]
        
        -- V3: Salidas hacia salas inexistentes
        errV3 = [ ErrorSemantico ("Error V3: La sala '" ++ sID ++ "' apunta a una salida inexistente: '" ++ dest ++ "'")
                | (sID, Salida _ dest _) <- todasSalidas, notElem dest setSalas ]
                
        -- V4: Salidas que requieren un objeto inexistente
        errV4 = [ ErrorSemantico ("Error V4: Salida en '" ++ sID ++ "' requiere el objeto inexistente: '" ++ req ++ "'")
                | (sID, Salida _ _ (Just req)) <- todasSalidas, notElem req setObjs ]
                
        -- V5: Sala con dos salidas en la misma dirección
        errV5 = [ ErrorSemantico ("Error V5: La sala '" ++ sID ++ "' tiene múltiples salidas en la dirección " ++ show dir)
                | Sala sID _ sals _ <- listaSalas
                , let dirs = [d | Salida (Just d) _ _ <- sals]
                , dir <- encontrarDuplicados dirs ]
                
        -- V6: Sala que contiene un objeto no definido globalmente
        errV6 = [ ErrorSemantico ("Error V6: La sala '" ++ sID ++ "' contiene el objeto no definido: '" ++ oID ++ "'")
                | Sala sID _ _ objs <- listaSalas, oID <- objs, notElem oID setObjs ]
                
        -- V7: Objeto colocado en más de una sala (o repetido en la misma)
        dupsObjetosSalas = encontrarDuplicados todosObjsEnSalas
        errV7 = [ ErrorSemantico ("Error V7: El objeto '" ++ oID ++ "' está colocado en más de una sala o repetido.")
                | oID <- dupsObjetosSalas ]
                
        -- V8: INICIO o META con sala u objeto inexistentes
        Cabecera _ iniMeta metaDef _ = cabecera mundo
        errInicio = [ErrorSemantico ("Error V8: La sala INICIO '" ++ iniMeta ++ "' no existe.") | notElem iniMeta setSalas]
        errMeta = case metaDef of
            LlegarA mSala -> [ErrorSemantico ("Error V8: La sala META '" ++ mSala ++ "' no existe.") | notElem mSala  setSalas]
            LlevarA mObj mSala ->  
                [ErrorSemantico ("Error V8: El objeto META '" ++ mObj ++ "' no existe.") | notElem mObj setObjs]
                ++ [ErrorSemantico ("Error V8: La sala META '" ++ mSala ++ "' no existe.") | notElem mSala setSalas]
                
        -- Extraemos la sala destino de la META (funciona tanto para LlegarA como para LlevarA)
        metaSala = case metaDef of
            LlegarA mS -> mS
            LlevarA _ mS -> mS
                
        -- V9: Sala no alcanzable desde INICIO (ignorando requisitos de objetos)
        alcancibles = bfs [iniMeta] (grafoAdyacencia listaSalas) []
        errV9 = [ ErrorSemantico ("Error V9: La sala '" ++ sID ++ "' no es alcanzable desde INICIO.")
                | sID <- setSalas, notElem sID alcancibles ]
--        dbg1 = unsafePerformIO (print ("SALAS ALCANCIBLES: " ++ show alcancibles))
--        dbg2 = unsafePerformIO (print ("SALA META BUSCADA: " ++ show metaSala))
--        dbg3 = unsafePerformIO (print ("GRAFO COMPLETO: " ++ show (grafoAdyacencia listaSalas)))

--        errV9 = [ ErrorSemantico ("Error V9: La sala META '" ++ metaSala ++ "' no es alcanzable desde INICIO.")
--                | notElem metaSala alcancibles ]

                
        -- V10: Un objeto es válido si está en una sala O si lo lleva el jugador en el inventario guardado
        objsNecesarios = [req | (_, Salida _ _ (Just req)) <- todasSalidas] ++ 
                         case metaDef of { LlevarA mObj _ -> [mObj]; _ -> [] }
        errV10 = [ ErrorSemantico ("Error V10: El objeto '" ++ oID ++ "' es necesario pero no está colocado en ninguna sala.")
                 | oID <- objsNecesarios
                 , elem oID setObjs
                 , notElem oID todosObjsEnSalas
                 , notElem oID invArchivo -- Tampoco lo tiene el jugador en el inventario
                 ]
        
        errPesoObjs = [ ErrorSemantico ("Error Peso: El objeto '" ++ oID ++ "' pesa " ++ show p ++ " y supera la capacidad maxima de la cabecera (" ++ show maxCap ++ ")")
                      | Objeto oID _ p <- listaObjs, p > maxCap ]
                      
        pesoInvArchivo = sum [p | Objeto oID _ p <- listaObjs, elem oID invArchivo]
        errCapacidadInv = [ ErrorSemantico ("Error Capacidad: El peso total del inventario guardado (" ++ show pesoInvArchivo ++ ") supera la capacidad maxima (" ++ show maxCap ++ ").")
                          | pesoInvArchivo > maxCap ]
                      
      in errPesoObjs <> errCapacidadInv <> errV3 <> errV4 <> errV5 <> errV6 <> errV7 <> errInicio <> errMeta <> errV9 <> errV10 
--    in dbg1 `seq` dbg2 `seq` dbg3 `seq` (errV3 <> errV4 <> errV5 <> errV6 <> errV7 <> errInicio <> errMeta <> errV9 <> errV10 <> errPesoObjs)


-- Encuentra elementos duplicados en una lista
encontrarDuplicados :: Eq a => [a] -> [a]
encontrarDuplicados [] = []
encontrarDuplicados (x:xs)
    | elem x xs = x : encontrarDuplicados (filter (/= x) xs)
    | otherwise   = encontrarDuplicados xs

-- grafo contruido con listas donde cada casilla-sala tendra a sus vecinos-salidas en otra lista asociada
type Grafo = [(String, [String])]

-- Construye un grafo de adyacencia de salas a partir de las salidas. Por cada sala, arma una tupla
grafoAdyacencia :: [Sala] -> Grafo
grafoAdyacencia salas = 
    [ (sID, [dest | Salida _ dest _ <- sals]) | Sala sID _ sals _ <- salas ]

-- Busqueda en anchura (BFS) para calcular salas alcanzables (V9)
bfs :: [String] -> Grafo -> [String] -> [String]
bfs [] _ visitados = visitados
-- xs es lo que esta pendiente
bfs (x:xs) grafo visitados
    | elem x visitados = bfs xs grafo visitados
    | otherwise = 
    -- Busca en el grafo la tupla correspondiente al nodo x
        let vecinos = case lookup x grafo of
                        Nothing -> []
                        Just v  -> v
            -- Filtramos los que ya visitamos o los que ya están en la cola de pendientes
            nuevos = filter (\v -> notElem v visitados && notElem v xs) vecinos
        in bfs (xs ++ nuevos) grafo (visitados ++ [x])
