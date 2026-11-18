# sobre comandos

## D1 formato de comando
Contexto: como parsear los comandos para poder procesarlos

Opciones: crear un intermediario que los parsee a un formato estandar (cmd, arg1) o directamente hacer casos para cada uno en crudo

Decision: crear un intermediario que los parsee a un formato estandar (cmd, arg1)

Consecuencias: ahora cuando el jugador colocar "ir <direcion>" o solo "<direcion>" nosotros lo interpretamos siempre como "ir <direcion>"


## D2 Representacion interna del mundo
Contexto: necesitamos una forma de que el programa pueda interactuar con el mundo, es decir, saber en que sala estamos, a cual podemos ir, los objetos que hay, etc...

Opciones: crear un tipo "Mundo" donde metamos toda la informacion al estilo de un struct en C, o... realmente no consideramos mas nada, entiendi que se podira hacer una lista donde nosotros llevemos orden de donde estan los elementos pero esta forma es dificil de ver en un lenguaje funcional

Decisiones: rear el tipo de dato Mundo

Consecuencias: ahora tenemos un tipo de dato que tiene toda la informacion del mundo separadita y bien organizada, (se puede ver como si fuese un arbol), asi que leer o escribir informacion en el es muy facil


## D3 Variantes del mundo
Contexto: elegir las variantes que tendra el mundo, si bien estaba obligado que hubiera por lo menos 1, podiamos colocar las que quisieramos

Opciones: solo colocar la obligatoria, o colocar otras

Decisiones: solo colocamos la obligatoria. si bien la estructura que creamos de mundo permite añadir algunas otras casi sin problemas, ya hay un cansancio de lenguajes funcionales jiji

Consecuencias: no hay trabajo extra :)


## D4 Como Reportar todos los errores
Contexto: debemos reportar todos los errores que consigamos (V1.. V10) si9n pararnos en uno solo

Opciones: crear una lista de errores o irlos lanzando uno a uno, es decir, una funcion impura

Deciciones: crear una lista de errores, como: pues primero tenemos un caso especial para V1, pues de el puede depender la deteccion de otros errores, luego es que nos metemos a ver cada unio de los errores por separado, si encontramos uno, encadenamos a la lista

Consecuencias: escalable y facil lectura

## D5 Diseño de reconocedores
Contexto: como caramba vamos a reconocer el mundo

Opciones: rococer todo de una vez (tratar de reconocer el mundo entero en una sola pasada), reconocer linea por linea 

Decisiones: nosotros reconocemos linea por linea y la entndemos de tres formas diferentes: una linea puede un separador, una linea vacia o de comentario, y una linea del estilo Clave - valor, por ejemplo, MUNDO: el tren infinito

Concecuencias: primero, esto hace muy facil guardar el mundo el la estructura especial que creamos y reconocer cuando estamos en que bloque, y esto fue muy importante porque el formato estandar de los mundos (el que el profesor va a correr) es inconsistente y no del todo util con los separadores, pues al momento de crear el Mundo, no le hacemos caso a los separadores sino a orden en el que nos encontramos con ciertos identificadores


## D6 Como obtenemos el numero de linea de un error
contexto: como hacemos para decirle al jugador en donde el mundo que desea cargar no es valido

Opciones: llevar la cuenta de la linea en la que vamos analizando el mundo linea por liena, y no se nos ocurrio otra forma

Decisiones: llevar la linea en la que vamos analizando simplmente en una funcion recursiva que recibe el acumulador y lo va aumentando

Consecuencias: ahora el jugador puede ver que es lo que provaca que el mundo no sea valido (pos ni modo)

## D7 la variante de CAPACIDAD
Contexto: 1910668 + 1810649 = 3721317 mod 4 = 1, nos toco la variante de CAPACIDAD, y teenmos que decidir como manejamos los casos en los que el jugador tenga o no la posibilidad de llevar los objetos requeridos para ganar encima

Opciones: la verdad es que aqui no hay muchas opciones, esta que si o si debe poder llevar los objetos necesarios para poder ganar y eso es todo, o, por ejemplo, darle la posibilidad de quedarce sin a posibilidad e moverse si carga cosas que superen su capacidad

Decisiones: pero bueno eleginos la simple, si o si debe poder llevar los objetos necesarios parapoder ganar y eso es todo

Consecuencias: menos trabajo :)


## D8 decision general de simplicidad
Conexto: se daba la libertad de hacer muchas cosas adicionales para el proyecto, lo cual proponia ponerle cariño al proyecto

Opciones: ponerse cariñoso, o dejarlo todo lo mas simple posible (hacer un MVP pues)

Decisiones: lo dejamos todo lo mas simple posible, pues, (y esto lo esta opinando Gaston) para llamarse un lenguaje de paradigma funcional, es muy dificil, de parte del que programador, hacer que el programa funcione, esa broma de las purezas he impuresas es un dolor de

Consecuencias: poder entregar el proyecto a tiempo