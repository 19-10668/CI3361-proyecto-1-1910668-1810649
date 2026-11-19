# Declaracion del uso de IA

**Autora:** Renata Colon, carnet 1810649

**Nota:** Mi compañero prefiere no utilizar inteligencia artificial, por lo que esta declaracion refleja unicamente mi experiencia

---

## Como use Gemini AI durante el proyecto

Utilice **Gemini AI** como apoyo para entender mensajes de error al compilar (sobre todo cuando no coincidian los casos que  esperabamos o cuando mezclaba tabulaciones con espacios sin darme cuenta), revisar partes de mi codigo, aclarar dudas de clase que surgian mientras avanzaba, orientarme cuando me sentia perdida entre tantas lineas y para la construccion de las pruebas de spec.hs

### Un ejemplo donde me ayudo y un error que tuve que corregir

Para detectar salas a las que no se podia llegar desde el inicio, sabia que debia revisar las conexiones como un mapa de lugares y caminos. Pense en recorrerlo a fondo o por niveles, y me incline por recorrerlo por niveles porque muestra el camino mas corto desde el punto de partida hacia cada lugar alcanzable. Como no me resultaba sencillo escribir esa idea en este estilo de programacion, la IA me ayudo a darle forma. Sin embargo, al probarlo en el programa, siempre fallaba y se quedaba en el punto inicial porque yo lo llamaba pasando el lugar de partida ya marcado como visitado; como el recorrido marca lo visitado y revisa solo lo que falta para pasar a los siguientes lugares, ese detalle bloqueaba el avance

### Un ejemplo donde rechace o descarte una sugerencia

Para juntar todos los errores de sentido del mundo en una sola lista, yo tenia en mente hacerlo concatenando listas paso a paso, mientras que Gemini me proponia ir mostrando los resultados de forma impura a medida que aparecian. Decidi seguir mi propia idea porque me parecio mas comoda y ordenada para lo que estaba haciendo

### Otras situaciones y aprendizajes

En varias ocasiones estuve editando un archivo equivocado y al probar el programa no veia reflejados mis cambios; ante eso, la IA me sugeria tocar varias lineas que en realidad estaban bien, y como no tenia sentido, busque por mi cuenta hasta darme cuenta de que archivo estaba tocando en verdad. En otros momentos, cuando aparecian complicaciones con ciertas herramientas o partes del codigo, resolvimos el problema revisando los casos posibles paso a paso, en vez de usar las soluciones complicadas o el uso de otras librerias que mencionaba Gemini
