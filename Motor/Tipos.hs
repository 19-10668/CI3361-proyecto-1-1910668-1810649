module Motor.Tipos (
    Comando (..),
    -- Objeto (..),
    -- Direccion (..),
    -- Sala (..),
    IdSala,
    -- Mundo,
    -- Meta (..),
    -- Estado (..),
    ErrorMundo (..),
    LineaParseada (..)
) where

import qualified Data.Map as M

-- representa el comando ingresado por el usuario (ej: "ir norte", "tomar llave")
data Comando = Comando {
    cmd  :: String,
    arg1 :: String
} deriving (Show, Eq)

-- identificador único para cada sala del mundo
type IdSala = String

-- representa un objeto interactuable dentro del juego
-- data Objeto = Objeto {
--     nombreObj :: String,
--     descObj   :: String
-- } deriving (Show, Eq)

-- direcciones posibles para los desplazamientos
-- data Direccion = Norte | Sur | Este | Oeste | Arriba | Abajo
--     deriving (Show, Eq, Read)

-- estructura de una sala con su descripción, salidas y objetos presentes
-- data Sala = Sala {
--     descSala    :: String,
--     salidas     :: M.Map Direccion IdSala,
--     objetosSala :: [Objeto]
-- } deriving (Show, Eq)

-- el mundo del juego representado como un mapa de identificadores a salas para prevenir que existan dos salas con el mismo
-- identificador
-- type Mundo = M.Map IdSala Sala

-- representa la meta del juego (para cumplir con: llegar a <sala> o llevar <objeto> a <sala>)
-- data Meta = LlegarA IdSala
--           | LlevarA IdSala String
--     deriving (Show, Eq)

-- estado global de la partida requerido
-- data Estado = Estado {
--     tituloMundo :: String,
--     salaActual  :: IdSala,   -- al inicio es la sala donde empieza
--     inventario  :: [Objeto],
--     mundoActual :: Mundo,
--     juegoActivo :: Bool,
--     metaJuego   :: Meta
-- } deriving (Show, Eq)

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