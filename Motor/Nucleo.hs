module Motor.Nucleo (verificarMeta,
    pasarMundo,
    extraerInventarioGuardado,
    extraerInicio,
    soltar,
    tomar,
    escogerSalida,
    mostrarObjeto,
    mostrarSala,
    str2Dir) where

import Motor.Tipos
import Data.List (delete)
import Control.Monad (forM, forM_)
import Data.Char (toLower)

buscarSala:: Mundo -> ID_Sala -> Sala
buscarSala mn id = foldl (foldSala) (Sala id "esa sala no existe" [] []) ((\(Bloque_Salas x) -> x) (salas mn))

mostrarSala:: Mundo -> ID_Sala -> String
mostrarSala mn id = 
    let salaActual = buscarSala mn id
    in "estas en " ++ id ++ ": " ++ (\(Sala a b c d)->b)salaActual ++ "\n" ++
    "en la sala hay: " ++ show ((\(Sala a b c d)->d)salaActual) ++ "\n" ++
    "y puedes ir hacia: " ++ show ((\(Sala a b c d)->c)salaActual) ++ "\n"

buscarObjeto::  Mundo -> Int -> Estado -> ID_Objeto -> Objeto
buscarObjeto mn cual es id
    | cual == 1 = foldl (foldObjeto) (Objeto id "No existe tal objeto" 1) ((\(Bloque_Objetos x) -> x) (objetos mn))
    | cual == 2 = foldl (foldObjeto) (Objeto id "No existe tal objeto" 1) ((\(Bloque_Objetos x) -> x) (objetos mn) ++ (inventario es))
    | cual == 3 = foldl (foldObjeto) (Objeto id "No existe tal objeto" 1) ((inventario es))



mostrarObjeto:: Mundo -> ID_Objeto -> Int -> Estado -> String
mostrarObjeto mn id cual es = (\(Objeto id d _)-> d) (buscarObjeto mn cual es id )


--buscarSalida:: Sala -> Direccion -> ID_Sala
--buscarSalida mn dir = salaDestino (foldl (foldSalida) (Salida {dirSalida = Just dir, salaDestino = "", reqObjeto = Just ""}) ((\(Sala a b c d) ->c)mn))
--buscarSalida aplicaba salaDestino (...) por fuera del foldl, devolviendo únicamente el nombre de la sala destino
buscarSalida :: Sala -> Direccion -> Salida
buscarSalida mn dir = foldl (foldSalida) (Salida {dirSalida = Just dir, salaDestino = "", reqObjeto = Nothing}) ((\(Sala a b c d) -> c) mn)

--escogerSalida :: Mundo -> ID_Sala -> Direccion -> Estado -> Estado
--escogerSalida mn sala dir es = Estado {current = (buscarSalida (buscarSala mn sala) dir), inventario = inventario es, capacidad = capacidad es}

escogerSalida :: Mundo -> ID_Sala -> Direccion -> Estado -> (Estado, String)
escogerSalida mn sala dir es =
    let sal = buscarSalida (buscarSala mn sala) dir
        destino = salaDestino sal
        idsInv = [oID | Objeto oID _ _ <- inventario es]
    in if destino == ""
       then (es, "No hay salida en esa direccion.")
       else case reqObjeto sal of
              Nothing -> (es {current = destino}, "Avanzas hacia " ++ destino ++ ".")
              Just req -> if elem req idsInv
                          then (es {current = destino}, "Usas '" ++ req ++ "' y avanzas hacia " ++ destino ++ ".")
                          else (es, "Salida bloqueada. Necesitas: " ++ req ++ ".")

str2Dir:: String -> Direccion
str2Dir s 
    | s == "norte" = Norte
    | s == "sur" = Sur
    | s == "este" = Este
    | s == "oeste" = Oeste
    | s == "arriba" = Arriba
    | s == "abajo" = Abajo

pesoTotal :: [Objeto] -> Int
pesoTotal objs = sum [p | Objeto _ _ p <- objs]


tomar :: Mundo -> ID_Objeto -> Estado -> (Mundo, Estado, String)
tomar mn id es = 
    let salaActual = buscarSala mn (current es)
        objetosEnSala = (\(Sala _ _ _ objs) -> objs) salaActual
    in if not (elem id objetosEnSala)
       then (mn, es, "Ese objeto no esta en la sala.")
       else 
           let obj = buscarObjeto mn 1 es id
               (Objeto _ _ pesoObj) = obj
               pesoActual = pesoTotal (inventario es)
           in if pesoActual + pesoObj > capacidad es
              then (mn, es, "No puedes llevar tanto peso (capacidad maxima: " ++ show (capacidad es) ++ ").")
              else (Mundo {
                       cabecera = cabecera mn,
                       objetos = objetos mn,
                       salas = tomarPoner (salas mn) mn (current es) es id
                   }, es {inventario = inventario es ++ [obj]}, "Has tomado: " ++ id)

tomarPoner:: Bloque_Salas -> Mundo -> ID_Sala -> Estado -> ID_Objeto -> Bloque_Salas
tomarPoner (Bloque_Salas salas) mn id es ido=
    let old = buscarSala mn id
        old_ID_Sala = (\(Sala a b c d) -> a) old
        old_Desc = (\(Sala a b c d) -> b) old
        old_Salidas = (\(Sala a b c d) -> c) old
        old_objetos = (\(Sala a b c d) -> d) old
        new_Sala = Sala old_ID_Sala old_Desc old_Salidas ( delete ( (\(Objeto n d p) -> n) (buscarObjeto mn 1 es ido))  ((\(Sala a b c d)->d) (buscarSala mn (current es))))
    in Bloque_Salas ( (delete old salas) ++ [new_Sala] )

soltar:: Mundo -> ID_Objeto -> Estado ->(Mundo, Estado)
soltar mn id es = (Mundo {
    cabecera = cabecera mn,
    objetos = objetos mn,
    salas = (soltarPoner (salas mn) mn (current es) es id)}, es {inventario = (delete (buscarObjeto mn 3 es id) (inventario es))})



soltarPoner:: Bloque_Salas -> Mundo -> ID_Sala -> Estado -> ID_Objeto -> Bloque_Salas
soltarPoner (Bloque_Salas salas) mn id es ido=
    let old = buscarSala mn id
        old_ID_Sala = (\(Sala a b c d) -> a) old
        old_Desc = (\(Sala a b c d) -> b) old
        old_Salidas = (\(Sala a b c d) -> c) old
        old_objetos = (\(Sala a b c d) -> d) old
        new_Sala = Sala old_ID_Sala old_Desc old_Salidas (old_objetos ++ [((\(Objeto n d p) -> n) (buscarObjeto mn 3 es ido))])
    in Bloque_Salas ( (delete old salas) ++ [new_Sala] )

extraerInventarioGuardado :: String -> Mundo -> [Objeto]
extraerInventarioGuardado contenido mundo =
    let ls = lines contenido
        invLineas = [drop 11 l | l <- ls, take 11 l == "INVENTARIO:"]
    in case invLineas of
         (val:_) -> let w = words val
                    in if null w || map toLower (head w) `elem` ["vacio", "ninguno"]
                       then []
                       else [buscarObjeto mundo 1 (Estado "" [] 0) idObj | idObj <- w]
         [] -> [] -- Si la palabra clave "INVENTARIO:" no aparece en absoluto, se entiende que está vacío



foldSala:: Sala -> Sala -> Sala
foldSala b a  
    | (\(Sala id n o m)-> id) b == (\(Sala id n o m)-> id) a = a
    | otherwise = b

foldObjeto:: Objeto -> Objeto -> Objeto
foldObjeto b a 
    | (\(Objeto id d p)-> id) b == (\(Objeto id d p)-> id) a = a
    | otherwise = b

foldSalida:: Salida -> Salida -> Salida 
foldSalida b a 
    | (\(Salida dir sala obj)-> dir) b == (\(Salida dir sala obj)-> dir) a = a
    | otherwise = b


extraerCabecera:: Mundo -> Cabecera
extraerCabecera m = cabecera m

extraerInicio:: Mundo -> ID_Sala
extraerInicio m = salaInicio (extraerCabecera m)


pasarMundo:: Mundo -> Estado -> String
pasarMundo mn es = 
    let titulo = "MUNDO: " ++ tituloMundo (cabecera mn) ++ "\n"
        inicio = "INICIO: " ++ current es ++ "\n"
        meta = pasarMeta (metaMundo (cabecera mn))
        capStr = "CAPACIDAD: " ++ show (capMax (cabecera mn)) ++ "\n"
        invStr = let ids = [oID | Objeto oID _ _ <- inventario es]
                 in if null ids 
                    then "INVENTARIO: vacio\n" 
                    else "INVENTARIO: " ++ unwords ids ++ "\n"
        totalObjetos = (\(a,b) -> b) (pasarObjetos( ((\(Bloque_Objetos x) -> x)(objetos mn)), ""))
        totalSalas = (\(a,b) -> b) (pasarSalas (((\(Bloque_Salas x) -> x)(salas mn)), ""))
    in titulo ++ inicio ++  meta ++ capStr ++ invStr ++ totalObjetos ++ totalSalas

{-
pasarObjetos:: ([Objeto], String) -> ([Objeto], String)
pasarObjetos ([], acc) = ([], acc)
pasarObjetos ((x:xs), acc) = pasarObjetos (xs, acc ++ "---\n" ++ "OBJETO: " ++ ((\(Objeto o d)-> o)x) ++ "\n" ++ "DESC: " ++ ((\(Objeto o d)-> d)x) ++ "\n") -}

-- para guardar el peso del objeto
pasarObjetos:: ([Objeto], String) -> ([Objeto], String)
pasarObjetos ([], acc) = ([], acc)
pasarObjetos ((x:xs), acc) = 
    let oID = (\(Objeto o _ _) -> o) x
        desc = (\(Objeto _ d _) -> d) x
        p = (\(Objeto _ _ w) -> w) x
        lineaPeso = if p == 1 then "" else "PESO: " ++ show p ++ "\n"
    in pasarObjetos (xs, acc ++ "---\n" ++ "OBJETO: " ++ oID ++ "\n" ++ "DESC: " ++ desc ++ "\n" ++ lineaPeso)

pasarSalas:: ([Sala], String) -> ([Sala], String)
pasarSalas ([], acc) = ([], acc)
pasarSalas ((x:xs), acc) = pasarSalas (xs, acc ++ "---\n" ++ "SALA: " ++ (\(Sala a b c d) -> a) x ++ "\n"
    ++ "DESC: " ++ (\(Sala a b c d) -> b) x ++ "\n"
    ++ (\(a, b) -> b) (pasarSalidas ((\(Sala a b c d) -> c) x, ""))
    ++ (\(a, b) -> b) (pasarNombreObjeto ((\(Sala a b c d) -> d) x, "")))

pasarSalidas:: ([Salida], String) -> ([Salida], String)
pasarSalidas ([], acc) = ([], acc)
pasarSalidas ((x:xs), acc) 
    | reqObjeto x == Nothing = pasarSalidas (xs, acc ++ "SALIDA: " ++ (formatearDir (dirSalida x)) ++ " -> " ++ (salaDestino x) ++ "\n")
    | reqObjeto x /= Nothing = pasarSalidas (xs, acc ++ "SALIDA: " ++ (formatearDir (dirSalida x)) ++ " -> " ++ (salaDestino x) ++ " requiere " ++ (\(Just z)->z)(reqObjeto x) ++ "\n") 

pasarNombreObjeto :: ([ID_Objeto], String) -> ([ID_Objeto], String)
pasarNombreObjeto ([], acc) = ([], acc)
pasarNombreObjeto ((x:xs), acc) = pasarNombreObjeto (xs, acc ++ "OBJETO: " ++ x ++ "\n")

formatearDir:: Maybe Direccion -> String
formatearDir (Just x)
    | x == Norte = "norte"
    | x == Sur = "sur"
    | x == Este = "este"
    | x == Oeste = "oeste"
    | x == Arriba = "arriba"
    | x == Abajo = "abajo"



pasarMeta :: Meta -> String
pasarMeta (LlegarA sala) = "META: llegar a " ++ sala ++ "\n"
pasarMeta (LlevarA obj sala) = "META: llevar " ++ obj ++ " a " ++ sala ++ "\n"

verificarMeta :: Mundo -> Estado -> Bool
verificarMeta mn es =
    let idsInv = [oID | Objeto oID _ _ <- inventario es]
    in case metaMundo (cabecera mn) of
        LlegarA salaMeta ->
            current es == salaMeta
        LlevarA objMeta salaMeta ->
            current es == salaMeta && elem objMeta idsInv
