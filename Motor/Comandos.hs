module Motor.Comandos (leerComando) where
import Motor.Tipos (Comando (Comando, cmd, arg1))
import Data.Char (toLower)

leerComando :: String -> Maybe Comando
leerComando [] = Nothing
leerComando s = case words s of
    [] -> Nothing
    (hRaw:c) ->
        let h = map toLower hRaw
        in case () of
            _ | h == "salir"      -> Just Comando {cmd = "salir", arg1 = ""}
              | h == "mirar"      -> Just Comando {cmd = "mirar", arg1 = ""}
              | h == "inventario" || h == "inv" -> Just Comando {cmd = "inventario", arg1 = ""}
              | h == "ayuda"      -> Just Comando {cmd = "ayuda", arg1 = ""}
              | h `elem` ["norte", "sur", "este", "oeste", "arriba", "abajo"] ->
                    Just Comando {cmd = "ir", arg1 = h}
              | h == "tomar" || h == "coger" -> case c of
                    (x:_) -> Just Comando {cmd = "tomar", arg1 = map toLower x}
                    _     -> Nothing
              | h == "soltar" -> case c of
                    (x:_) -> Just Comando {cmd = "soltar", arg1 = map toLower x}
                    _     -> Nothing
              | h == "examinar" -> case c of
                    (x:_) -> Just Comando {cmd = "examinar", arg1 = map toLower x}
                    _     -> Nothing
              | h == "guardar" -> case c of
                    (x:_) -> Just Comando {cmd = "guardar", arg1 = x}
                    _     -> Nothing
              | h == "cargar" -> case c of
                    (x:_) -> Just Comando {cmd = "cargar", arg1 = x}
                    _     -> Nothing
              | h == "ir" -> case c of
                    (x:_) -> direccionCorrecta (map toLower x)
                    _     -> Nothing
              | otherwise -> Nothing

direccionCorrecta :: String -> Maybe Comando
direccionCorrecta [] = Nothing
direccionCorrecta a
    | a == "norte" = Just Comando {cmd = "ir", arg1 = a}
    | a == "sur"   = Just Comando {cmd = "ir", arg1 = a}
    | a == "este"  = Just Comando {cmd = "ir", arg1 = a}
    | a == "oeste" = Just Comando {cmd = "ir", arg1 = a}
    | a == "arriba"= Just Comando {cmd = "ir", arg1 = a}
    | a == "abajo" = Just Comando {cmd = "ir", arg1 = a}
    | otherwise    = Nothing
