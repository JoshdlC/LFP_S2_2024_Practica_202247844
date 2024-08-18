program Practica1
    implicit none
    !* Declaracion de variables
    
    character(len=15), dimension(100) :: nombres
    integer, dimension(100) :: cantidades
    real, dimension(100) :: precios
    character(len=15), dimension(100) :: ubicaciones
    character(len=1) :: tab = achar(9)
    integer :: total_items = 0


    !* Llamada a la funcion mostrarMenu
    call mostrarMenu()

contains 

    !! Subrutinas
    subroutine mostrarMenu()
        implicit none
        integer :: opcion = 0
        print *, '=================================================='
        print *, 'Practica 1 - Lenguajes Formales y de Programacion' 
        print *, '=================================================='
        print *, '# Sistema de inventario'
        print *, ''
        print *, '1. Cargar inventario inicial '
        print *, '2. Cargar instrucciones de movimientos'
        print *, '3. Crear informe de inventario'
        print *, '4. Mostrar informacion estudiante'
        print *, '5. Salir'
        print *, ''
        print *, 'Ingrese una opcion: '

        do while (opcion /= 5)

            read *, opcion
            select case(opcion)
                case(1)
                    print *, ''
                    !print *, 'Cargar inventario inicial'
                    !?nombre del archivo inventario.inv
                    print *, ''
                    call cargarInv()
                case(2)
                    print *, ''
                    !print *, 'Cargar instrucciones de movimientos'
                    !?nombre del archivo instrucciones.mov
                    !call cargarInstrucciones()
                    print *, ''
                case(3)
                    print *, ''
                    print *, 'Crear informe de inventario'
                    print *, ''
                case(4)
                    print *, ''
                    call mostrarInfoEstudiante()
                case(5)
                    print *, ''
                    print *, 'Saliendo del programa'
                    stop !!termina el programa
                case default
                    print *, 'Opcion no valida'
            end select
        end do
    end subroutine mostrarMenu

    subroutine mostrarInfoEstudiante()
        print *, '==================================================='
        print *, 'Nombre: Josue Samuel de la Cruz Medina'
        print *, 'Carnet: 202247844'
        print *, 'Lab Lenguajes Formales y de Programacion seccion B+'
        print *, '==================================================='
        print *, ''
        call mostrarMenu()
    end subroutine mostrarInfoEstudiante


    !* subrutina para cargar el inventario inicial
    subroutine cargarInv()

        implicit none
        integer :: io, ios, cantidad, i
        real :: precio
        character(len=15) :: nombre, ubicacion
        character(len=100) :: linea
        character(len=20) :: cantidad_str, precio_str
        character(len=12) :: reservada
        character(len=20) :: precio_temp
        integer :: pos_coma

        print *, '------------------------------'
        print *, 'Cargando inventario inicial...'
        print *, '------------------------------'
        print *, ''
        !! Logica para cargar el inventario inicial de archivo inventario.inv
        open (newunit = io, file = "inventario.inv", status="old", iostat=ios) ! * abre archivo
        !! verifica si existe el archivo
        if (ios /= 0) then 
            print *, "Error: No se puede abrir el archivo 'inventario.inv'. Verifique que el archivo existe."
            return !* Vuelve al menú
        end if

        print *, 'Nombre' //tab// " " //tab// 'Cantidad' //tab// " " //tab// 'Precio' //tab// " " //tab// 'Ubicacion'
        print *, '-----------------------------------------------------------------'
        i = 0

        do
            read (io, '(A)', iostat=ios) linea
            if (ios /= 0) exit !! Sale del bucle si hay un error al leer

            !* Extraer la palabra reservada "crear_equipo"
            read (linea(1:12), '(A12)', iostat=ios) reservada
            if (ios /= 0) then
                print *, "Error: Error al leer la palabra reservada."
                exit
            end if

            !* Verificar si la palabra reservada es correcta, sino sale de la subrutina
            if (reservada /= "crear_equipo") then
                print *, "Error: Formato de archivo incorrecto. Se esperaba la palabra reservada 'crear_equipo'."
                exit
            end if

            !! Procesar el resto de la línea
            linea = adjustl(linea(13:)) !* Elimina "crear_equipo" y ajusta la línea

            !! Buscar la primera coma para extraer el nombre
            pos_coma = index(linea, ';') !* encuentra el primer punto y coma que sigue en la línea
            if (pos_coma > 0) then !* si se encontró el punto y coma
                nombre = trim(linea(1:pos_coma-1)) !* extrae el nombre que está antes del punto y coma
                linea = adjustl(linea(pos_coma+1:)) !* Elimina la parte ignorada y ajusta la línea
            end if

            !! Leer la cantidad
            pos_coma = index(linea, ';')
            if (pos_coma > 0) then
                read (linea(1:pos_coma-1), *) cantidad
                linea = adjustl(linea(pos_coma+1:)) !* Elimina la parte ignorada y ajusta la línea
            end if

            !! Leer el precio (cambiado a F10.2 para manejar números más grandes)
            pos_coma = index(linea, ';')
            if (pos_coma > 0) then
                precio_temp = trim(linea(1:pos_coma-1)) !* Extrae la parte del precio
                read (precio_temp, '(F10.2)') precio !* Lee el precio usando el formato adecuado
                linea = adjustl(linea(pos_coma+1:)) !* Elimina la parte ignorada y ajusta la línea

            end if

            !! Leer la ubicación
            ubicacion = trim(linea)

            !! Almacena en arrays
            i = i + 1
            nombres(i) = nombre
            cantidades(i) = cantidad
            precios(i) = precio
            ubicaciones(i) = ubicacion

            write (cantidad_str, '(I3)') cantidad
            write (precio_str, '(F10.2)') precio
            print *, nombre // " " // cantidad_str // " " // precio_str // " " // ubicacion

        end do

        total_items = i

        close(io) !* Cierra el archivo
        print *, ''
        print *, 'Carga exitosa!'
        print *, '____________________________________________'
        print *, ''


        call mostrarMenu()



    end subroutine cargarInv
        
    !* subrutina para cargar las instrucciones de movimientos
    subroutine cargarInstrucciones()
        integer :: io, ios, cantidad
        real :: precio
        character(len=15) :: nombre, ubicacion, cantidad_str, precio_str
        character(len=100) :: linea
        character(len=20) :: temporal

        print *, '---------------------------'
        print *, 'Cargando instrucciones...'
        print *, '---------------------------'
        print *, ''
        !! Logica para cargar el inventario inicial de archivo instruccioens.mov
        !* abre archivo de instrucciones
        open (newunit = io, file = "instrucciones.mov", status="old", iostat=ios)
        
        !! verifica si existe el archivo
        if (ios /= 0) then 
            print *, "Error: No se puede abrir el archivo 'instrucciones.mov'. Verifique que el archivo existe."
            return !* Vuelve al menú
        end if

        do 
            read (io, *, iostat=ios) linea
            if (ios /= 0) then !* Si hay error o es fin del archivo, sale del bucle
                print *, ''
                print *, 'Carga exitosa!'
                print *, ''
                call mostrarMenu()
            end if
        
        
        print *, 'Carga exitosa!'
        print *, ''

        end do

    end subroutine cargarInstrucciones
end program Practica1

 
