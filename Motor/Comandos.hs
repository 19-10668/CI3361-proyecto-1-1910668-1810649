module Comandos () where


import Tipos (Comamdo)

leerComando :: [String] -> Maybe Comamdo
leerComando [] = Nothing
leerComando (h,t) = just h