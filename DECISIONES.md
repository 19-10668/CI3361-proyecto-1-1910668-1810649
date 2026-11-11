## sobre comandos

* como todos los comandos son "comando - argumento", decidimos que sera solo de esa forma, es decir, todos los comandos o tiene un solo argumento o ninguo, y eso lo representamos en una estructura de datos Comando con campos cmd :: String y arg1 :: String
* como se debe poder soportar escribir comandos de direciones sin el "ir", todos los comandos de direcciones se interpretaran internamente en la estructura Comando como cmd = "ir", arg1 = "la_direccion"
