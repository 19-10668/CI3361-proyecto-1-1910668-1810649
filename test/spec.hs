module Main where

import Test.HUnit
import Motor.Tipos
import Motor.Mundo (leerMundo)
import Motor.Comandos (leerComando)
import Motor.Nucleo
  ( verificarMeta
  , extraerInicio
  , soltar
  , tomar
  , escogerSalida
  , str2Dir
  )
import Motor.Persistencia (leerArchivo)
  
-- 1. PRUEBAS DE leerMundo (ajusta rutas y cantidad de errores)
pruebasMundo :: [Test]
pruebasMundo =
  [ TestCase $ do
      -- Caso 1: mundo de ejemplo valido
      posibleTxt <- leerArchivo "mundos/entrega.txt"
      case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/entrega.txt"
        Just txt -> case leerMundo txt of
          Right _   -> return ()
          Left errs -> assertFailure ("Mundo de ejemplo invalido: " ++ show errs)

  , TestCase $ do
      -- Caso 2: "El Misterio de la mansion Sombras"
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt"
        Just txt -> case leerMundo txt of
          Right mn  -> assertEqual "Sala inicial debe ser vestibulo" "vestibulo" (extraerInicio mn)
          Left errs -> assertFailure ("Tu mundo propio falló: " ++ show errs)

  , TestCase $ do
      -- Caso 3: mundo invalido 1 (sintactico)
      posibleTxt <- leerArchivo "mundos/invalidos/mundoErrSintactico.txt"
      case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/invalidos/mundoErrSintactico.txt"
        Just txt -> case leerMundo txt of
          Left errs -> assertEqual "Cantidad de errores en mundoErrSintactico.txt" 5 (length errs)
          Right _   -> assertFailure "Se esperaba error en mundoErrSintactico.txt"

  , TestCase $ do
      -- Caso 4: mundo invalido 2 (semantico)
      posibleTxt <- leerArchivo "mundos/invalidos/mundoerr1.txt"
      case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/invalidos/mundoerr1.txt"
        Just txt -> case leerMundo txt of
          Left errs -> assertEqual "Cantidad de errores en mundoerr1.txt" 3 (length errs)
          Right _   -> assertFailure "Se esperaba error en mundoerr1.txt"

  , TestCase $ do
      -- Caso 5: mundo invalido 3 (semantico)
      posibleTxt <- leerArchivo "mundos/invalidos/mundoerr2.txt"
      case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/invalidos/mundoerr2.txt"
        Just txt -> case leerMundo txt of
          Left errs -> assertEqual "Cantidad de errores en mundoerr2.txt" 4 (length errs)
          Right _   -> assertFailure "Se esperaba error en mundoerr2.txt"
  ]

-- 2. PRUEBAS DE leerComando (validos e invalidos)
pruebasComandos :: [Test]
pruebasComandos =
  [ TestCase $ assertBool "Cadena vacia debe dar Nothing"
      (leerComando "" == Nothing)

  , TestCase $ assertBool "Comando desconocido debe dar Nothing"
      (leerComando "comando_inexistente" == Nothing)

  , TestCase $ assertEqual "Comando 'mirar' valido"
      (Just (Comando { cmd = "mirar", arg1 = "" }))
      (leerComando "mirar")

  , TestCase $ assertEqual "Comando 'tomar lupa' válido"
      (Just (Comando { cmd = "tomar", arg1 = "lupa" }))
      (leerComando "tomar lupa")

  , TestCase $ assertEqual "Comando 'ir norte' válido"
      (Just (Comando { cmd = "ir", arg1 = "norte" }))
      (leerComando "ir norte")
  ]

-- 3. PRUEBAS PURAS DEL NUCLEO (secuencias sin IO: bloqueo, meta, variante)
pruebasNucleo :: [Test]
pruebasNucleo =
  [ TestCase $ do
      -- Caso 11: chocar con salida bloqueada ('este' en salon_principal requiere llave_antigua)
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      mn <- case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt" >> return undefined
        Just txt -> case leerMundo txt of
          Left errs -> assertFailure ("Mundo invalido: " ++ show errs) >> return undefined
          Right m   -> return m
      let st0 = Estado { current = "salon_principal", inventario = [], capacidad = capMax (cabecera mn) }
          (st1, msg) = escogerSalida mn "salon_principal" (str2Dir "este") st0
      assertEqual "No debe avanzar si falta llave_antigua" "salon_principal" (current st1)
      assertBool "Debe informar salida bloqueada" (not (null msg))

  , TestCase $ do
      -- Caso 12: tomar lupa en vestibulo y luego soltarla
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      mn <- case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt" >> return undefined
        Just txt -> case leerMundo txt of
          Left errs -> assertFailure ("Mundo invalido: " ++ show errs) >> return undefined
          Right m   -> return m
      let st0 = Estado { current = "vestibulo", inventario = [], capacidad = capMax (cabecera mn) }
          (mn1, st1, _)  = tomar mn "lupa" st0
          (_mn2, st2, _) = soltar mn1 "lupa" st1
      assertEqual "Inventario queda vacío tras soltar lupa" [] (inventario st2)

  , TestCase $ do
      -- Caso 13: intentar tomar un objeto que no está en la sala actual
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      mn <- case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt" >> return undefined
        Just txt -> case leerMundo txt of
          Left errs -> assertFailure ("Mundo invalido: " ++ show errs) >> return undefined
          Right m   -> return m
      let st0 = Estado { current = "vestibulo", inventario = [], capacidad = capMax (cabecera mn) }
          (_mn1, st1, msg) = tomar mn "carta_confesion" st0
      assertEqual "Inventario sigue vacío" [] (inventario st1)
      assertBool "Informa que no está en la sala" (not (null msg))

  , TestCase $ do
      -- Caso 14: recorrido parcial que intenta cruzar salida bloqueada sin llave_antigua
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      mn <- case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt" >> return undefined
        Just txt -> case leerMundo txt of
          Left errs -> assertFailure ("Mundo invalido: " ++ show errs) >> return undefined
          Right m   -> return m
      let cap = capMax (cabecera mn)
          st0 = Estado { current = "vestibulo", inventario = [], capacidad = cap }
          -- vestibulo -> salon_principal -> tomar guantes_latex
          (st1, _)        = escogerSalida mn "vestibulo" (str2Dir "norte") st0
          (mn1, st2, _)   = tomar mn "guantes_latex" st1
          -- salon_principal -> invernadero -> jardin_trasero -> tomar linterna_uv
          (st3, _)        = escogerSalida mn1 "salon_principal" (str2Dir "oeste") st2
          (st4, _)        = escogerSalida mn1 "invernadero" (str2Dir "norte") st3
          (mn2, st5, _)   = tomar mn1 "linterna_uv" st4
          -- jardin_trasero -> invernadero -> salon_principal -> intentar ir este sin llave_antigua
          (st6, _)        = escogerSalida mn2 "jardin_trasero" (str2Dir "sur") st5
          (st7, _)        = escogerSalida mn2 "invernadero" (str2Dir "este") st6
          (st8, msg)      = escogerSalida mn2 "salon_principal" (str2Dir "este") st7
      assertEqual "Debe quedarse en salon_principal sin llave_antigua" "salon_principal" (current st8)
      assertBool "Debe mostrar mensaje de bloqueo" (not (null msg))
      assertBool "No debe haber ganado aun" (not (verificarMeta mn2 st8))

  , TestCase $ do
      -- Caso 15: prueba llegar al final
      posibleTxt <- leerArchivo "mundos/mundo.txt"
      mn <- case posibleTxt of
        Nothing  -> assertFailure "No existe mundos/mundo.txt" >> return undefined
        Just txt -> case leerMundo txt of
          Left errs -> assertFailure ("Mundo invalido: " ++ show errs) >> return undefined
          Right m   -> return m
      let cap = capMax (cabecera mn)
          st0 = Estado { current = "vestibulo", inventario = [], capacidad = cap }
          -- vestibulo -> norte (salon_principal) -> tomar guantes_latex
          (st1, _)       = escogerSalida mn "vestibulo" (str2Dir "norte") st0
          (mn1, st2, _)  = tomar mn "guantes_latex" st1
          -- salon_principal -> oeste (invernadero) -> tomar llave_antigua
          (st3, _)       = escogerSalida mn1 "salon_principal" (str2Dir "oeste") st2
          (mn2, st4, _)  = tomar mn1 "llave_antigua" st3
          -- invernadero -> norte (jardin_trasero) -> tomar linterna_uv
          (st5, _)       = escogerSalida mn2 "invernadero" (str2Dir "norte") st4
          (mn3, st6, _)  = tomar mn2 "linterna_uv" st5
          -- jardin_trasero -> sur (invernadero) -> este (salon_principal) -> este (biblioteca)
          (st7, _)       = escogerSalida mn3 "jardin_trasero" (str2Dir "sur") st6
          (st8, _)       = escogerSalida mn3 "invernadero" (str2Dir "este") st7
          (st9, _)       = escogerSalida mn3 "salon_principal" (str2Dir "este") st8
          -- soltar llave_antigua para liberar peso y tomar codigo_cifrado
          (mn4, st10, _) = soltar mn3 "llave_antigua" st9
          (mn5, st11, _) = tomar mn4 "codigo_cifrado" st10
          -- biblioteca -> oeste (salon_principal) -> norte (pasillo_oscuro) -> arriba (atico) -> tomar carta_confesion
          (st12, _)      = escogerSalida mn5 "biblioteca" (str2Dir "oeste") st11
          (st13, _)      = escogerSalida mn5 "salon_principal" (str2Dir "norte") st12
          (st14, _)      = escogerSalida mn5 "pasillo_oscuro" (str2Dir "arriba") st13
          (mn6, st15, _) = tomar mn5 "carta_confesion" st14
          -- atico -> abajo (pasillo_oscuro) -> sur (salon_principal) -> arriba (despacho_principal)
          (st16, _)      = escogerSalida mn6 "atico" (str2Dir "abajo") st15
          (st17, _)      = escogerSalida mn6 "pasillo_oscuro" (str2Dir "sur") st16
          (st18, _)      = escogerSalida mn6 "salon_principal" (str2Dir "arriba") st17
      assertEqual "Debe llegar a despacho_principal" "despacho_principal" (current st18)
      assertBool "Debe cumplirse verificarMeta" (verificarMeta mn6 st18)
  ]

main :: IO ()
main = runTestTTAndExit $ TestList
  (pruebasMundo ++ pruebasComandos ++ pruebasNucleo)
