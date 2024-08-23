program Practica1
    implicit none
    !* Declaracion de variables
    
    character(len=15), dimension(200) :: nombres
    integer, dimension(200) :: cantidades
    real, dimension(200) :: precios
    character(len=20), dimension(200) :: ubicaciones
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
                    call cargarInstrucciones()
                    print *, ''
                case(3)
                    print *, ''
                    print *, 'Crear informe de inventario'
                    print *, ''
                    call crearInforme()
                case(4)
                    print *, ''
                    call mostrarInfoEstudiante()
                case(5)
                    print *, ''
                    print *, 'Saliendo del programa...'
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


    !! subrutina para cargar el inventario inicial
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
                nombre = trim(adjustl(linea(1:pos_coma-1))) !* extrae el nombre que está antes del punto y coma
                !print *, nombre,'|||'
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
            nombres(i) = trim(adjustl(nombre))
            cantidades(i) = cantidad
            precios(i) = precio
            ubicaciones(i) = trim(adjustl(ubicacion))

            write (cantidad_str, '(I3)') cantidad
            write (precio_str, '(F10.2)') precio
            print *, nombre // " " // cantidad_str // " " // precio_str // " " // ubicacion

        end do

        total_items = i

        ! do i = 1, total_items
        !     print *, "------"
        !     print *, nombres(i)//"|"
        !     print *, cantidades(i)
        !     print *, precios(i)
        !     print *, ubicaciones(i)
        !     print *, "------"
        ! end do

        close(io) !* Cierra el archivo
        print *, ''
        print *, 'Carga exitosa!'
        print *, '____________________________________________'
        print *, ''


        call mostrarMenu()



    end subroutine cargarInv
        
    !! subrutina para cargar las instrucciones de movimientos
    subroutine cargarInstrucciones()
        integer :: io, ios, cantidad, i, find, numlinea
        character(len=150) :: linea, linea_original
        character(len=20) :: temporal
        character(len=15) :: ubicacion
        integer :: pos_punto1, pos_punto2, cantidad_total
        character(len=15) :: cantidad_str, nombre
        numlinea = 0
        print *, '---------------------------'
        print *, 'Cargando instrucciones...'
        print *, '---------------------------'
        print *, ''
        !! Logica para cargar el inventario inicial de archivo instrucciones.mov
        !* abre archivo de instrucciones
        open (newunit = io, file = "instrucciones.mov", status="old", iostat=ios)
        
        !! verifica si existe el archivo
        if (ios /= 0) then 
            print *, "Error: No se puede abrir el archivo 'instrucciones.mov'. Verifique que el archivo existe."
            return !* Vuelve al menú
        end if
    
        print *, '--------------------------------------------------------------------------------------'
    
        do 
            read (io, '(A)', iostat=ios) linea_original !* lee una línea del archivo io abierto
            if (ios /= 0) then 
                exit !* Sale del bucle si hay error o es fin del archivo
            end if
            
            numlinea = numlinea + 1
            !* Buscar primeras instrucciones de lineas
            !temporal = adjustl(linea(1:len_trim(linea)))
            linea_original = trim(adjustl(linea_original))

            if (len_trim(linea_original) == 0) then !* omite las líneas en blanco
                cycle
            end if

            !print *, 'Linea: ', numlinea
            !print *, 'Linea original: ', linea_original

            !* Extraer la palabra reservada
            temporal = trim(adjustl(linea_original))

            !* Encontrar el primer espacio para separar la palabra reservada
            pos_punto1 = index(temporal, ' ')
            if (pos_punto1 > 0) then
                temporal = trim(adjustl(temporal(1:pos_punto1-1))) !* palabra reservada
                !print *,  '|', pos_punto1, '|'
                !print *, 'Linea completa: ', linea_original
                print *, ''

                ! linea = trim(linea(1:pos_punto1)) !* resto de la línea
                linea = trim(adjustl(linea_original(pos_punto1+1:)))
    
                print *, 'Palabra reservada: '//temporal
                print *, 'Resto de la linea: ', linea
                
                !* Verificar si la palabra reservada es correcta
                if (temporal == "agregar_stock" .or. temporal == "eliminar_equipo") then !* si la palabra reservada es correcta
                    pos_punto1 = index(linea, ';')
                    if (pos_punto1 > 0) then !* si se encontró el punto y coma
                        nombre = trim(adjustl(linea(1:pos_punto1-1))) !* se obtiene el nombre
                        linea = trim(adjustl(linea(pos_punto1+1:)))
    
                        pos_punto2 = index(linea, ';')
                        if (pos_punto2 > 0) then !* si se encontró el punto y coma despues del nombre
                            cantidad_str = trim(adjustl(linea(1:pos_punto2-1))) !* se obtiene la cantidad
                            ubicacion = trim(adjustl(linea(pos_punto2+1:))) !* se obtiene la ubicación
                            read(cantidad_str, *) cantidad !* Lee la cantidad
    
                            !* Verificar y actualizar inventario
                            find = 0
                            do i = 1, total_items !* Busca el equipo en el inventario
                                if (nombre == nombres(i) .and. ubicacion == ubicaciones(i)) then !* si el nombre y la ubicación coinciden
                                    if (temporal == "agregar_stock") then !* si se agrega stock
                                        cantidades(i) = cantidades(i) + cantidad  !* añade cantidad al stock
                                        print *, "Se agrego", cantidad, "al stock de ", nombre, "en la ubicacion ", ubicacion
                                    else if (temporal == "eliminar_equipo") then !* si se elimina stock
                                        if (cantidades(i) >= cantidad) then
                                            cantidades(i) = cantidades(i) - cantidad !* elimina cantidad del stock
                                            ! do j = i, total_items-1 !* Elimina el equipo, pero no debe ser asi
                                            !     nombres(j) = nombres(j+1)
                                            !     cantidades(j) = cantidades(j+1)
                                            !     precios(j) = precios(j+1)
                                            !     ubicaciones(j) = ubicaciones(j+1)
                                            ! end do
                                            print *, "Se elimino", cantidad, "del stock de ", nombre, "en la ubicacion ", ubicacion
                                        else
                                            print *, ""
                                            print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                            print *, "  Error: No se puede eliminar mas de la cantidad existente."
                                            print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                            print *, ""
                                            exit
                                        end if
                                    end if
                                    find = 1 !* Marca que se encontró el equipo
                                    exit !* Sale del bucle al encontrar y procesar el equipo
                                ! else if (nombre /= nombres(i)) then !* si no se encuentra el equipo
                                !     print *, ""
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, "  Error: No se encontro el equipo"
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, ""
                                ! else if (ubicacion /= ubicaciones(i)) then !* si la ubicación no coincide
                                !     print *, ""
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, "  Error: La ubicacion no coincide"
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, ""
                                end if
                            end do
    
                            if (find == 0) then !* si no se encontro el equipo
                                ! if (nombre /= nombres(i)) then !* si no se encuentra el equipo
                                !     print *, ""
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, "  Error: No se encontro el equipo"
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, ""
                                ! else if (ubicacion /= ubicaciones(i)) then !* si la ubicación no coincide
                                !     print *, ""
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, "  Error: La ubicacion no coincide"
                                !     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                !     print *, ""
                                ! end if
                                print *, ""
                                print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                print *, "  Error: No se encontro el equipo y/o la ubicacion no coincide."
                                print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                print *, ""
                                exit
                            end if
                        
                        end if
                    end if
                else !* sino es una palabra reservada correcta
                    print *, ""
                    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                    print *, "  Error: Palabra reservada incorrecta. Se esperaba 'agregar_stock' o 'eliminar_equipo'."
                    print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                    print *, ""
                end if

            end if
            
        end do
        close(io)
        print *, ''
        
        print *, ''
        call mostrarMenu()
    end subroutine cargarInstrucciones
    
    


    !! subrutina para crear informe.txt
    subroutine crearInforme()
        implicit none
        integer :: io, ios, cantidad, i
        real :: precio
        character(len=15) :: cantidad_str, precioUni_str, precioTotal_str

        print *, '------------------'
        print *, 'Creando informe...'
        print *, '------------------'

        !* abrir un archivo .txt para escribir el informe
        open (newunit = io, file = "informe.txt", status="replace", iostat=ios)

        !* verifica si se puede abrir el archivo
        if (ios /= 0) then 
            print *, "Error: No se puede abrir el archivo 'informe.txt'."
            return !* Vuelve al menú
        end if

        !* escribe el encabezado
        write (io, '(A)') 'Informe de inventario:'
        write (io, '(A)') ''
        write (io, '(A)') 'Nombre' //tab// " " //tab// 'Cantidad' //tab// " " //tab// 'Precio Unitario ($)' //tab// " " //tab// 'Precio total ($)' //tab// " "//tab// 'Ubicacion'
        write (io, '(A)') '-------------------------------------------------------------------------------------------'
        
        do i = 1, total_items
            write (cantidad_str, '(I3)') cantidades(i)
            write(precioUni_str, '(A,F10.2)') '$',precios(i)
            write(precioTotal_str, '(A,F10.2)') '$',cantidades(i) * precios(i)
            write (io, '(A,A,A,A,A)') trim(adjustl(nombres(i))) //tab// " " //tab// adjustl(cantidad_str) //tab// " " //tab// precioUni_str //tab// " " //tab// precioTotal_str //tab// " " //tab// ubicaciones(i)
        end do

        close(io) !* Cierra el archivo
        print *, ''
        call mostrarMenu()
    end subroutine crearInforme

    !! subrutina para parsear el comando
    ! subroutine parsear()
    !     character(len=256) :: reservada
    !     character(len=50) :: nombre
    !     integer :: cantidad
    !     real :: precio_unitario
    !     character(len=50) :: ubicacion

    !     character(len=256) :: temp_reservada
    !     integer :: pos_coma
    !     character(len=50) :: cantidad_str, precio_unitario_str

    !     !* Extraer el nombre y el poder del comando
    !     temp_reservada = reservada(len_trim(reservada)+2:) !* Remover la parte del comando ("create", "injury", o "power")
    !     pos_coma = index(temp_reservada, ",")
    ! end subroutine parsear
end program Practica1

 
