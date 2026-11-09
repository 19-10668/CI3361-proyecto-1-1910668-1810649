module Motor.Comandos (leerComando) where


import Motor.Tipos (Comando (Comando, cmd, arg1))

leerComando :: String -> Maybe Comando
leerComando [] = Nothing
leerComando s = case words s of
    [] -> Nothing
    (h:c)
        | h == "salir" -> Just Comando {cmd = "salir", arg1 = ""}
        | otherwise -> Nothing