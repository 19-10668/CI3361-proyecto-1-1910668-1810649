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
                
--- Lógica de Construcción

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
    -- aca tampoco va a fallar head pues la salida que estamos parseando es una cosa que existe ya
    -- asi que words da aunque sea algo (la direccion)
        dirStr = head w
        dir = stringToDir dirStr
        destino = w !! 2
        req = if  elem "requiere" w
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

-- Funcion auxiliar recursiva que lleva el contador 'n' de la línea
validarSintaxis :: Int -> [String] -> [ErrorMundo]
validarSintaxis _ [] = []
validarSintaxis n (linea:resto) =
    let limpia = trim linea
    -- head aqui no falla. la razon es el corto circuito
    in if null limpia || head limpia == '#' || limpia == "---"
       then validarSintaxis (n + 1) resto  -- Ignorar líneas vacías, comentarios o separadores
       else 
           let errorMsg = clasificarErrorSintaxis linea
           in if not (null errorMsg)
              then [ErrorSintaxis n errorMsg] ++ validarSintaxis (n + 1) resto
              else validarSintaxis (n + 1) resto
                     
clasificarErrorSintaxis :: String -> String
clasificarErrorSintaxis linea
    | not (elem ':' linea) = "Linea sin la forma CLAVE: valor"
    | otherwise =
        let (claveCruda, valorCrudo) = separarDosPuntos linea
            clave = trim claveCruda
            valor = trim valorCrudo
        in if notElem clave ["MUNDO", "INICIO", "META", "OBJETO", "DESC", "SALA", "SALIDA"]
           then "Clave desconocida: " ++ clave
           else if elem clave ["SALA", "OBJETO"] && (null valor || any isSpace valor)
                then "Identificador invalido: no debe contener espacios (use guion bajo '_')"
                else if clave == "META"
                     then 
                         let palabras = words valor
                         in if null palabras || 
                               (head palabras == "llegar" && length palabras /= 3) || 
                               (head palabras == "llevar" && length palabras /= 4) ||
                               notElem (head palabras) ["llegar", "llevar"]
                            then "Meta mal formada. Use: 'META: llegar a [sala]' o 'META: llevar [objeto] a [sala]'"
                            else ""
                     else if clave == "SALIDA"
                          then 
                              -- Tomamos la primera palabra del valor de la salida
                              let palabras = words valor
                              in if null palabras 
                                 then "Salida mal formada: falta la direccion"
                                 else 
                                     -- head aqui tampoco falla, pues ya verificamos que palabras no sea null
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
        
-- usamos el monoide de las listas [] para acumular y encadenar todos los errores.       
validarSemantica :: Mundo -> [ErrorMundo]
validarSemantica mundo = 
    validarIDsDuplicados mundo <> validarReferencias mundo

-- V2: IDs duplicados (Salas u Objetos)

validarIDsDuplicados :: Mundo -> [ErrorMundo]
validarIDsDuplicados mundo = 
    let (Bloque_Objetos listaObjs) = objetos mundo
        (Bloque_Salas listaSalas) = salas mundo
        
        idsObjs = [oID | Objeto oID _ <- listaObjs]
        idsSalas = [sID | Sala sID _ _ _ <- listaSalas]
        
        duplicadosObjs = encontrarDuplicados idsObjs
        duplicadosSalas = encontrarDuplicados idsSalas
        
        errsObjs = [ErrorSemantico ("Error V2: Objeto duplicado: " ++ o) | o <- duplicadosObjs]
        errsSalas = [ErrorSemantico ("Error V2: Sala duplicada: " ++ s) | s <- duplicadosSalas]
    in errsObjs ++ errsSalas

-- V3 a V10

validarReferencias :: Mundo -> [ErrorMundo]
validarReferencias mundo = 
    let (Bloque_Objetos listaObjs) = objetos mundo
        (Bloque_Salas listaSalas) = salas mundo
        
        setObjs = [oID | Objeto oID _ <- listaObjs]
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
        Cabecera _ iniMeta metaDef = cabecera mundo
        errInicio = [ErrorSemantico ("Error V8: La sala INICIO '" ++ iniMeta ++ "' no existe.") | notElem iniMeta setSalas]
        errMeta = case metaDef of
            LlegarA mSala -> [ErrorSemantico ("Error V8: La sala META '" ++ mSala ++ "' no existe.") | notElem mSala  setSalas]
            LlevarA mObj mSala -> 
                [ErrorSemantico ("Error V8: El objeto META '" ++ mObj ++ "' no existe.") | notElem mObj setObjs]
                ++ [ErrorSemantico ("Error V8: La sala META '" ++ mSala ++ "' no existe.") | notElem mSala setSalas]
                
        -- V9: Sala no alcanzable desde INICIO (ignorando requisitos de objetos)
        alcancibles = bfs [iniMeta] (grafoAdyacencia listaSalas) [iniMeta]
        errV9 = [ ErrorSemantico ("Error V9: La sala '" ++ sID ++ "' no es alcanzable desde INICIO.")
                | sID <- setSalas, notElem sID alcancibles ]
                
        -- V10: Objeto necesario (meta o requisito de salida) pero no colocado en ninguna sala
        objsNecesarios = [req | (_, Salida _ _ (Just req)) <- todasSalidas] ++ 
                         case metaDef of { LlevarA mObj _ -> [mObj]; _ -> [] }
        errV10 = [ ErrorSemantico ("Error V10: El objeto '" ++ oID ++ "' es necesario pero no está colocado en ninguna sala.")
                 | oID <- objsNecesarios
                 , notElem oID todosObjsEnSalas ]

    in errV3 <> errV4 <> errV5 <> errV6 <> errV7 <> errInicio <> errMeta <> errV9 <> errV10

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
