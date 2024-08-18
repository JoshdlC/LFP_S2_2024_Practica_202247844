program Practica1
    implicit none
    !* Declaracion de variables
    


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

        integer :: io, ios, cantidad
        real :: precio
        character(len=15) :: nombre, ubicacion, cantidad_str, precio_str
        character(len=100) :: linea
        character(len=20) :: temporal

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

        do 
            read (io, *, iostat=ios) linea
            if (ios /= 0) exit !* Si hay error o es fin del archivo, sale del bucle

            read(linea, '(A35)') temporal !* lee los primeros 35 caracteres de la linea en que se va
            linea = adjustl(linea(len_trim(temporal) + 2:)) !* Elimina la parte ignorada y ajusta la línea

            !* Luego, extrae el nombre, cantidad, precio y ubicación
            ! Lee y separa la línea en las variables correspondientes
            read(linea, '(A15, I3, F7.2, A)') nombre, cantidad, precio, ubicacion
            
            !? convierte los integer a string
            write(cantidad_str, '(I3)') cantidad
            write(precio_str, '(F6.2)') precio

            print *, 'Nombre' // " " // 'Cantidad' // " " // 'Precio' // " " // 'Ubicacion'
            print *, nombre // " " // cantidad_str // " " // precio_str // " " // ubicacion 

           
        end do

        print *, 'Carga exitosa!'
        print *, ''
        call mostrarMenu()



    end subroutine cargarInv
        
    !* subrutina para cargar las instrucciones de movimientos
    subroutine cargarInstrucciones()
        print *, '---------------------------'
        print *, 'Cargando instrucciones...'
        print *, '---------------------------'
        print *, ''
        !* Logica para cargar el inventario inicial de archivo instruccioens.mov
        
        print *, 'Carga exitosa!'
        print *, ''

    end subroutine cargarInstrucciones
end program Practica1

 
