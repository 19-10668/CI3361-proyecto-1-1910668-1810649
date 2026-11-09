module Motor.Comandos (leerComando) where


import Motor.Tipos (Comando (Comando, cmd, arg1))

leerComando :: String -> Maybe Comando
leerComando [] = Nothing
leerComando s = case words s of
    [] -> Nothing
    (h:a:c)
        | h == "salir" -> Just Comando {cmd = "salir", arg1 = ""}
        | h == "mirar" -> Just Comando {cmd = "mirar", arg1 = ""}
        | h == "tomar" || h == "coger" -> Just Comando {cmd = "tomar", arg1 = a}
        | h == "soltar" -> Just Comando {cmd = "soltar", arg1 = a}
        | h == "examinar" -> Just Comando {cmd = "examinar", arg1 = a}
        | h == "inventario" || h == "inv" -> Just Comando {cmd = "inventario", arg1 = ""}
        | h == "guardar" -> Just Comando {cmd = "guardar", arg1 = a}
        | h == "cargar" -> Just Comando {cmd = "cargar", arg1 = a}
        | h == "ayuda" -> Just Comando {cmd = "ayuda", arg1 = ""}
        | h == "norte" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "sur" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "este" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "oeste" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "arriba" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "abajo" -> Just Comando {cmd = "ir", arg1 = h}
        | h == "ir" -> direccionCorrecta (a)
        | otherwise -> Nothing

direccionCorrecta :: String -> Maybe Comando
direccionCorrecta [] = Nothing
direccionCorrecta a
    | a == "norte" = Just Comando {cmd = "ir", arg1 = a}
    | a == "sur" = Just Comando {cmd = "ir", arg1 = a}
    | a == "este" = Just Comando {cmd = "ir", arg1 = a}
    | a == "oeste" = Just Comando {cmd = "ir", arg1 = a}
    | a == "arriba" = Just Comando {cmd = "ir", arg1 = a}
    | a == "abajo" = Just Comando {cmd = "ir", arg1 = a}
    | otherwise = Nothing