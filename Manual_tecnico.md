# Manual técnico Práctica Única LFP
## Josué Samuel de la Cruz Medina - 202247844

### Introducción
El siguiente manual técnico es para que personas conocedoras de programación puedan familiarizarse con la estructura lógica de este programa.
El programa es un inventario de objetos u equipo que tienen su propia cantidad, precio y ubicación. 
Únicamente se puede cambiar la cantidad disponible de cada objeto u equipo.

### Estructura del código
Todo el código fue escrito en un solo archivo llamado main.f90

![imagen1](/LFP_S2_2024_Practica_202247844/Imagenes/imagen1.png)

### Contenidos carpeta LFP_S2_2024_Practica_202247844
Tenemos los archivos que va a usar el programa, siendo estos: 
1. Archivos de lectura
   1. Aqui podemos encontrar los archivos que va a leer el programa segun cada opción ingresada.
2. main.exe
   1. Es el ejecutable del programa.
3. main.f90
   1. Es el archivo de mi código de mi única clase donde se encuentra toda la lógica usada por nuestro programa.
4. Manuales
5. README.md
   1. Archivo README creado al momento de crear el repositorio.


### Clase principal main.f90
![clasePrincipal](/LFP_S2_2024_Practica_202247844/Imagenes/imagen2.png)
![clasePrincipal2](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen3.png)


Aqui podemos apreciar que nuestro código esta constituido por el programa Practica1 y nuestras subrutinas que se encuentran dentro del mismo programa:
1. mostrarMenu
2. mostrarInfoEstudiante
3. cargarInv
4. cargarInstrucciones
5. crearInforme

#### **Subrutina mostrarMenu()**
Tenemos la primer subrutina la cual utilizamos para escribir el menú principal en consola cuando ejecutamos nuestro programa. Con distintas opciones para que el usuario elija. Es recomendable que se siga en orden para una correcta ejecución del programa.
![subrutina mostrarMenu](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen4.png)
![subrutina mostrarMenu2](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen5.png)

Se crea la variable opcion, la cual se utiliza para que el usuario ingrese la opción que desea del Menú. Se imprime en consola nuestro Menú principal con las posibles opciones que pueda ingresar el usuario. Las primeras 4 opciones van a mandar a llamar subrutinas (o métodos), las cuales hacen el trabajo según su nombre.
Cuando el usuario ingresa su opción, se tiene un do que va a funcionar hasta que el usuario ingrese la opción 5. 

En la primer (1) opción se llama la subrutina cargarInv que cargará el inventario inicial para nuestro programa. 

La segunda (2) opción llama a la subrutina cargarInstrucciones que cargará el archivo de instrucciones para modificar la cantidad de objetos/equipo de nuestro programa.

La tercer (3) opción llama a la subrutina crearInforme, donde modificaremos nuestro archivo informe.txt que se encuentra vacio para imprimir el resultado de las anteriores dos opciones.

La cuarta (4) opción llama a la subrutina mostrarInfoEstudiante, donde se muestra la información de mi persona mediante la consola.

La quinta (5) opción sale del programa, haciendo uso del comando stop para terminarlo.

#### **Subrutina cargarInv**
![subrutina cargarInv](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen6.png)
![subrutina cargarInv2](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen7.png)
![subrutina cargarInv3](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen8.png)
![subrutina cargarInv4](/LFP_S2_2024_Practica_202247844/Imagenes/Imagen9.png)

En esta subrutina cargamos el archivo inventario.inv, el cual contiene el inventario inicial. Creamos las variables y abrimos el archivo de inventario.inv, lo leemos asegurandonos de que contenga la palabra reservada 'crear_equipo', seguida de un espacio y las características del objeto, nombre, cantidad, precio unitario y la ubicación. 

Separamos el contenido del archivo, lo imprimos en consola para asegurarnos de que está bien guardado y por último lo guardamos en arrays globales que tenemos. Luego de esto cerramos el archivo y regresamos al Menú principal.

##### Formato archivos de inventario inicial

![formato archivo inventario.inv](/LFP_S2_2024_Practica_202247844/Imagenes/imagen16.png)

#### Subrutina cargarInstrucciones
![subrutina cargarInstrucciones](/LFP_S2_2024_Practica_202247844/Imagenes/imagen10.png)
![subrutina cargarInstrucciones2](/LFP_S2_2024_Practica_202247844/Imagenes/imagen11.png)
![subrutina cargarInstrucciones3](/LFP_S2_2024_Practica_202247844/Imagenes/imagen12.png)
![subrutina cargarInstrucciones4](/LFP_S2_2024_Practica_202247844/Imagenes/imagen13.png)

En esta subrutina cargamos el archivo instrucciones.mov, el cual contiene la serie de cambios en las cantidades que van a tener algunos objetos de nuestro inventario inicial. Creamos las variables para almacenar temporalmente los datos leidos.

Nos aseguramos que se pueda abrir el archivo y revisamos que cada linea que se va leyendo tenga una de las 2 palabras reservadas: 'agregar_stock' y 'eliminar_equipo'. Ya que tengan una de las dos, guardamos el nombre, la cantidad y ubicacion de cada uno de los objetos.

Dependiendo de cada palabra reservada, nos vamos por un if ya sea para añadir la cantidad indicada o para eliminar la cantidad indicada. En dado caso el nombre y la ubicación del ultimo archivo cargado (instrucciones.mov) no sean los correctos, vamos a dar un mensaje de error y salir de la subrutina. Si la cantidad a eliminar es mayor de la que se tiene en el inventario inicial, va imprimir un error en consola y salirse de la subrutina. Si la cantidad es menor o igual a la que se tiene, se va a sumar/restar dependiendo de la palabra reservada, si el resultado termina en 0, el objeto no se va a eliminar.

##### Formato archivo de instrucciones

![formato archivo instrucciones](/LFP_S2_2024_Practica_202247844/Imagenes/imagen17.png)
![formato archivo instrucciones2](/LFP_S2_2024_Practica_202247844/Imagenes/imagen18.png)


#### **Subrutina crearInforme**

![subRutina crearInforme](/LFP_S2_2024_Practica_202247844/Imagenes/imagen14.png)

En está subrutina unicamente nos encargamos de modificar nuestro archivo informe.txt , el cual ya existe dentro de nuestra carpeta pero se encuentra en blanco.

Se crean las variables a usar, se abre el archivo informe.txt (se revisa que se pueda abrir), escribimos el encabezado en nuestro archivo y comenzamos a escribir linea por linea cada objeto que haya sido cargado y/o modificado en nuestras anteriores subrutinas. Con excepción de que se agrega la columna de valor total que multiplica la cantidad por el precio unitario de cada objeto/equipo.

Una vez terminados de recorrer los arrays se cierra el archivo y se regresa al Menú principal.

#### **Subrutina mostrarInfoEstudiante**

![subrutina mostrarInfoEstudiante](/LFP_S2_2024_Practica_202247844/Imagenes/imagen15.png)

Su único propósito es mostrar mi información, imprimiendo mi nombre, carnet y curso en consola. Luego de esto se regresa al menú principal.
