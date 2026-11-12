module Motor.Reconocedor 
  ( ReadS
  , leerLineaArchivo
  , quitarEspaciosInicio
  , imprimir
  , sacarLineaP
  ) where

import Data.Char (isSpace)
import Motor.Tipos 

-- definición de ReadS:
-- un reconocedor toma un String y devuelve una lista de pares (resultado, resto).
-- type ReadS a = String -> [(a, String)]



-- remueve los espacios al inicio de una cadena recursivamente
quitarEspaciosInicio :: String -> String
quitarEspaciosInicio [] = []
quitarEspaciosInicio (c:cs)
    | isSpace c = quitarEspaciosInicio cs
    | otherwise = c:cs

-- remueve los espacios al final de la cadena de forma recursiva
quitarEspaciosFin :: String -> String
quitarEspaciosFin [] = []
quitarEspaciosFin s
    | isSpace (last s) = quitarEspaciosFin (init s)
    | otherwise        = s

-- limpia espacios al inicio y al final
trim :: String -> String
trim s = quitarEspaciosFin (quitarEspaciosInicio s)

-- extrae el texto hasta encontrar un salto de linea '\n' o el final
separarHastaSalto :: String -> (String, String)
separarHastaSalto [] = ("", "")
separarHastaSalto ('\n':xs) = ("", xs)
separarHastaSalto (x:xs) = 
    let (val, resto) = separarHastaSalto xs
    in (x:val, resto)

-- verifica si una linea está compuesta unicamente por espacios en blanco
esSoloEspacios :: String -> Bool
esSoloEspacios [] = True
esSoloEspacios (c:cs)
    | isSpace c = esSoloEspacios cs
    | otherwise = False

-- verifica si el primer caracter relevante (saltando espacios) es '#'
esComentario :: String -> Bool
esComentario s = 
    let limpio = quitarEspaciosInicio s
    in not (null limpio) && head limpio == '#'
    -- el uso de head aqui es perfectamente seguro, es decir, head no dara errores, pues
    -- primero tamos verificando que limpio no este vacio. En el caso de que limpio este
    -- vacio, tonces ya todo el primer termino es falso, y como haskell tiene corto circuito
    -- no vamos a caer en head :P
    -- pero de todas formas haskell tira un warnig en head diciendo que es mejor usar pattern matching 

-- reconoce una palabra o identificador usando lex
leerToken :: ReadS String
leerToken = lex

-- combinador para leer los dos puntos ":"
leerDosPuntos :: ReadS String
leerDosPuntos s = 
    [ (t, r)
    | (t, r) <- lex s
    , t == ":"
    ]

-- combinador principal para una linea de tipo CLAVE: valor
leerClaveValor :: ReadS (String, String)
leerClaveValor s = 
    [ ((clave, trim valor), restoLinea)
    | (clave, r1) <- leerToken s
    , (_, r2) <- leerDosPuntos r1
    , let valorCrudo = quitarEspaciosInicio r2
    , let (valor, restoLinea) = separarHastaSalto valorCrudo
    ]

-- reconoce el separador de bloques "---"
leerSeparador :: ReadS String
leerSeparador s = 
    [ ("---", r)
    | (t, r) <- lex s
    , t == "---"
    ]

leerLineaArchivo :: ReadS LineaParseada
leerLineaArchivo s = 
    -- 1. intentar leer como Clave: Valor
    [ (LClave c v, r) | ((c, v), r) <- leerClaveValor s ]
    ++
    -- 2. intentar leer como Separador "---"
    [ (LSeparador, r) | (_, r) <- leerSeparador s ]
    ++
    -- 3. considerar linea vacia o comentario
    [ (LVacia, s) | esSoloEspacios s || esComentario s ]

sacarLineaP :: [(LineaParseada, String)] -> LineaParseada
sacarLineaP ((l,s):xs) = l

imprimir :: (LineaParseada, String) -> String
imprimir lista = impAux (fst lista)

impAux :: LineaParseada -> String
impAux (LClave k v) = k
impAux (LSeparador) = "separador"
impAux (LVacia) = "vacia"