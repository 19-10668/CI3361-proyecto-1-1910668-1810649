# redactado por Gastón García y Renata Colón con acuerdos que surgieron durante el desarrollo y el estudio del comportamiento del programa
# sobre comandos 

## D1 formato de comando
Contexto: como parsear los comandos para poder procesarlos.

Opciones: crear un intermediario que los parsee a un formato estandar (cmd, arg1) o directamente hacer casos para cada uno en crudo.

Decision: crear un intermediario que los parsee a un formato estandar (cmd, arg1), la unica abreviatura posible es inv en vez de inventario, pero ambas se pueden usar a gusto del consumidor.

Consecuencias: ahora cuando el jugador colocar "ir <direccion>" o solo "<direccion>" nosotros lo interpretamos siempre como "ir <direccion>".

## D2 Representacion interna del mundo
Contexto: necesitamos una forma de que el programa pueda interactuar con el mundo, es decir, saber en que sala estamos, a cual podemos ir, los objetos que hay, etc...

Opciones: crear un tipo "Mundo" donde metamos toda la informacion al estilo de un struct en C, o... realmente no consideramos mas nada, entendi que se podria hacer una lista donde nosotros llevemos orden de donde estan los elementos pero esta forma es dificil de ver en un lenguaje funcional.

Decisiones: rear el tipo de dato Mundo.

Consecuencias: ahora tenemos un tipo de dato que tiene toda la informacion del mundo separada y bien organizada, (se puede ver como si fuese un arbol), asi que leer o escribir informacion en este es muy facil.


## D3 Variantes del mundo
Contexto: elegir las variantes que tendra el mundo, si bien estaba obligado que hubiera por lo menos 1, podiamos colocar las que quisieramos.

Opciones: solo colocar la obligatoria, o colocar otras.

Decisiones: solo colocamos la obligatoria. si bien la estructura que creamos de mundo permite añadir algunas otras casi sin problemas, ya hay un cansancio de lenguajes funcionales jiji.

Consecuencias: no hay trabajo extra :).


## D4 Como Reportar todos los errores
Contexto: debemos reportar todos los errores que consigamos (V1..V10) sin pararnos en uno solo.

Opciones: crear y acumular una lista pura de errores, o ir mostrándolos uno a uno mediante una función impura a medida que aparecen.

Decisiones: creamos un estilo recursivo para detectar los errores de V1 (sintacticos), luego usamos la concatenacion de listas para cada error de V2 y de peso, no usamos el bloque do de Either porque se detiene en el primer Left y no evalúa las siguientes validaciones, impidiendo acumular todos los errores.

Consecuencias: escalable y facil lectura.

## D5 Diseño de reconocedores
Contexto: como caramba vamos a reconocer el mundo.

Opciones: rococer todo de una vez (tratar de reconocer el mundo entero en una sola pasada), reconocer linea por linea.

Decisiones: nosotros reconocemos linea por linea y la entendemos de tres formas diferentes: una linea puede un separador, una linea vacia o de comentario, y una linea del estilo Clave - valor, por ejemplo, MUNDO: el tren infinito.

Concecuencias: primero, esto hace muy facil guardar el mundo de la estructura especial que creamos y reconocer cuando estamos en que bloque, y esto fue muy importante porque el formato estandar de los mundos (el que el profesor va a correr) es inconsistente y no del todo util con los separadores, pues al momento de crear el Mundo, no le hacemos caso a los separadores sino al orden en el que nos encontramos con ciertos identificadores, por ejemplo en la cabecera: MUNDO, INICIO, META, CAPACIDAD.

## D6 Como obtenemos el numero de linea de un error
contexto: como hacemos para decirle al jugador en donde el mundo que desea cargar no es valido.

Opciones: llevar la cuenta de la linea en la que vamos analizando el mundo linea por linea, y no se nos ocurrio otra forma.

Decisiones: llevar la linea en la que vamos analizando simplemente en una funcion recursiva que recibe el acumulador y lo va aumentando.

Consecuencias: ahora el jugador puede ver que es lo que provoca que el mundo no sea valido (pos ni modo).

## D7 la variante de CAPACIDAD
Contexto: 1910668 + 1810649 = 3721317 mod 4 = 1, nos toco la variante de CAPACIDAD, y tenemos que decidir como manejamos los casos en los que el jugador tenga o no la posibilidad de llevar los objetos requeridos para ganar encima.

Opciones: la verdad es que aqui no hay muchas opciones, si o si debe poder llevar los objetos necesarios para poder ganar y eso es todo, o, por ejemplo, darle la posibilidad de quedarse sin la posibilidad de moverse si carga cosas que superen su capacidad.

Decisiones: pero bueno elegimos la simple, si o si debe poder llevar los objetos necesarios para poder ganar y eso es todo, si el usuario no da los pesos el programa lo interpreta como si pesara 1.

Consecuencias: menos trabajo :).


## D8 decision general de simplicidad
Contexto: se daba la libertad de hacer muchas cosas adicionales para el proyecto, lo cual proponia ponerle cariño al proyecto.

Opciones: ponerse cariñoso, o dejarlo todo lo mas simple posible (hacer un MVP pues).

Decisiones: lo dejamos todo lo mas simple posible, pues, (y esto lo esta opinando Gaston) para llamarse un lenguaje de paradigma funcional, es muy dificil, de parte del que programador, hacer que el programa funcione, esa broma de las purezas e impuresas es un dolor de...

Consecuencias: poder entregar el proyecto a tiempo T.T.

## D9: decision de Estado, copia del mundo o solo lo que cambia, donde quedan los objetos sueltos
Contexto: Estado debe guardar cómo va la partida actualmente para poder llevar un registro. Cuando se quiere tener la opcion de "guardar" el estado actual para acceder a el al colocar el comando "cargar" cómo deberia hacerse y si el usuario "suelta" un objeto dónde queda.

Opciones: no se nos ocurrieron muchas ideas para esto solo la que tomamos como definitiva en las decisiones.

Decisiones: Estado, guarda el identificador de la sala actual, el inventario como una lista de objetos y mantiene presente en su estructura la capacidad maxima de peso para siempre hacer las comprobaciones.
Se guarda el mundo tal cual como se otorgo pero se agrega una linea en la cabecera con el inventario actual, si tiene objetos guardados, los cuales dejan de mostrarse en las salas que los contenian originalmente. El nuevo inicio es donde actualmente el jugador se halla y al "soltar"/"tomar" un elemento de la sala se entiende como una modificacion del lugar correspondiente de la lista de salas y se elimina la misma por ser un estado viejo y se añade al final de la estructura el nuevo estado del lugar, por ello en el archivo guardado se encuentran en las ultimas lineas o en posiciones diferentes al que hacia referencia.
Los objetos sueltos quedan en la lista de objetos de la sala actual donde esta el jugador y se eliminan del inventario como consecuencia, lo cual permite cargar otras herramientas.

Consecuencias: cumple con las especificaciones y la carga del mundo.
