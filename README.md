## Proyecto Prolog - Materia Principios de Lenguajes de Programación
### Maestria en Ciencias Computacionales
En esta primera parte del proyecto se reconoce si un string provisto es o no es una expresión matemática básica en lenguaje Python. Escribir este reconocedor les ayudará a escribir la base del resto del proyecto.  Un string no es otra cosa que una secuencia de caracteres. Primero deben de poder agrupar estos caracteres basados en ciertas reglas que les permitirán tener ahora una secuencia de tokens. Por ejemplo, la expresión “result = oldsum - value / 100” resulta en la siguiente lista de tokens:         [ “result”, “=”, “oldsum”, “-“, “value”, “/”, “100” ]  Estas reglas para separar tokens no son muy complejas, existe una lista de caracteres bien especifica, en el ejemplo se puede observar que todo está separado limpiamente por espacios pero en general los operadores también pueden separarse de los otros tokens sin necesidad de whitespace.

Deben de escribir este tokenizer primero.

El siguiente paso es lograr identificar cada token. Para los operadores esto no es complicado. En el caso de identificadores tienen que buscar las reglas del lenguaje que les permiten nombrar identificadores. De esta manera logramos añadir información crucial a la lista anterior.

Identificador: result

Operador de Asignación: =

Identificador: result

Operador de Resta: -

Identificador: value

Operador de División: /

Literal Entero: 100

A continuación las reglas del lenguaje determinan como se combinan ciertos tokens para construir expresiones validas. La siguiente gramática describe operaciones matemáticas, respeta la precedencia de operaciones y permite el uso de paréntesis:

![Problema](https://user-images.githubusercontent.com/36687480/121258549-a8e0d300-c874-11eb-890c-aae9a802f5b2.png)

El objetivo es recibir un string e indicar si es o no es un <assignStmt> válido según esta gramática. En los siguientes avances deberán construir las piezas de la gramática que hacen el subset de Python un poco más complejo.

-Un programa debe de consistir de varias sentencias/statements.
  
-Iteraciones y condicionales con anidación.
  
-Definición de funciones.
  
-Manejo de listas y matrices
