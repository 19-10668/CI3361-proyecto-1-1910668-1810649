module Motor.Tipos (
    Comando (..),
    Objeto (..),
    Direccion (..),
    Sala (..),
    ID_Sala,
    ID_Objeto,
    Desc,
    Meta (..),
    Bloque_Objetos (..), 
    Bloque_Salas (..),
    Mundo (..),
    Estado(..),    
    -- Estado (..),
    ErrorMundo (..),
    LineaParseada (..),
    Cabecera (..),
    Salida (..)
) where


import qualified Data.Map as M

-- representa el comando ingresado por el usuario (ej: "ir norte", "tomar llave")
data Comando = Comando {
    cmd  :: String,
    arg1 :: String
} deriving (Show, Eq)

-- identificador único para cada sala del mundo
type ID_Sala = String
type ID_Objeto = String
type Desc = String


-- direcciones posibles para los desplazamientos
data Direccion = Norte | Sur | Este | Oeste | Arriba | Abajo
     deriving (Show, Eq, Read)

data Objeto = Objeto ID_Objeto Desc deriving (Show, Eq)
data Bloque_Objetos = Bloque_Objetos [Objeto] deriving (Show, Eq)


-- representa la meta del juego (para cumplir con: llegar a <sala> o llevar <objeto> a <sala>)
data Meta = LlegarA ID_Sala
           | LlevarA ID_Objeto ID_Sala
     deriving (Show, Eq)

data Mundo = Mundo{
    cabecera :: Cabecera,
    objetos :: Bloque_Objetos,
    salas :: Bloque_Salas
} deriving (Show, Eq)

data Salida = Salida {
    dirSalida   :: Maybe Direccion,
    salaDestino :: String,
    reqObjeto   :: Maybe String
} deriving (Show, Eq)

data Cabecera = Cabecera {
    tituloMundo :: String,
    salaInicio  :: ID_Sala,
    metaMundo   :: Meta
} deriving (Show, Eq)




data Sala = Sala ID_Sala Desc [Salida] [ID_Objeto] deriving (Show, Eq)
--data Salida = Salida String deriving (Show, Eq)
-- data Salida = Salida_libre String ID_Sala | Salida_compleja String ID_Sala ID_Objeto deriving (Show, Eq, Read)
data Bloque_Salas = Bloque_Salas [Sala] deriving (Show, Eq)

data Estado = Estado {
    current:: ID_Sala,
    inventario:: [Objeto],
    capacidad:: Int
} deriving (Show, Eq)


-- representa los diferentes errores posibles al validar el mundo (V1 a V10)/ bajo observacion
data ErrorMundo = ErrorSintaxis Int String
                | ErrorSemantico String
    deriving (Show, Eq)

-- representación intermedia de una linea analizada
data LineaParseada
  = LClave String String -- clave y Valor (ej: "MUNDO", "Entrega a última hora")
  | LSeparador           -- separador "---"
  | LVacia               -- linea en blanco o comentario
  deriving (Show, Eq, Read)
