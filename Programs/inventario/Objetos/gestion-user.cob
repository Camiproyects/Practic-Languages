       IDENTIFICATION DIVISION.
       PROGRAM-ID. gestion-user.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
      ****************************************************************
      *                GESTIÓN USUARIOS                              *
      *                                                              *
      * Descripción: Manejo de usuarios mediante archivo indexado.   *
      *              Se utilizan operaciones CRUD basadas en         *
      *              la clave NUMDOC.                                *
      *                                                              *
      * Autor: ANDRES CAMILO LAGUNA BERNAL                           *
      * Fecha: 02-02-2025                                            *
      ****************************************************************
       DATE-WRITTEN. 02-02-2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLI ASSIGN TO "../Usuarios/usuarios.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NUMDOC
               ALTERNATE RECORD KEY IS CODUNI WITH DUPLICATES
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-CLI.
       01  USUDATA.
           05 NOMAPE   PIC X(65).
           05 TIPDOC   PIC X(02).
           05 NUMDOC   PIC 9(10).
           05 MONTPA   PIC 9(10).
           05 NUMCON   PIC 9(10).
           05 CORREO   PIC X(30).
           05 CARGO    PIC X.
           05 DETALL   PIC X(65).
           05 FECREG   PIC 9(08).
           05 REDOND   PIC 9(10).
           05 CODUNI   PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-FS        PIC XX.
       01 LIM          PIC X.
       01 WS-OPCION    PIC X.

       LINKAGE SECTION.
       01 PASAR.
           07 T-NUMDOC   PIC 9(10).
           07 T-NOMAPE   PIC X(65).

       PROCEDURE DIVISION USING PASAR.
       INICIO.
           PERFORM CLEAR-SCREEN.
           OPEN I-O ARCHIVO-CLI
           IF WS-FS NOT = "00"
              DISPLAY "Error al abrir el archivo. FS = " WS-FS
              STOP RUN
           END-IF.
           PERFORM MENU-CRUD UNTIL WS-OPCION = "Q".
           CLOSE ARCHIVO-CLI.
           DISPLAY "PROGRAMA TERMINADO :p".
           STOP RUN.
       
       MENU-CRUD.
           PERFORM CLEAR-SCREEN.
           DISPLAY "-------------------------------" LINE 3 POSITION 20.
           DISPLAY "  MENU DE USUARIOS INDEXADOS   " LINE 4 POSITION 20.
           DISPLAY "-------------------------------" LINE 5 POSITION 20.
           DISPLAY "  C -> Crear usuario"       LINE 7 POSITION 20.
           DISPLAY "  R -> Leer usuario"        LINE 9 POSITION 20.
           DISPLAY "  U -> Actualizar usuario"  LINE 11 POSITION 20.
           DISPLAY "  D -> Eliminar usuario"    LINE 13 POSITION 20.
           DISPLAY "  Q -> Salir"               LINE 15 POSITION 20.
           DISPLAY "Seleccione una opción:"     LINE 17 POSITION 20.
           ACCEPT WS-OPCION LINE 17 POSITION 41.
           EVALUATE WS-OPCION
               WHEN "C" 
                    PERFORM OPERACION-CREAR
               WHEN "R" 
                    PERFORM OPERACION-LEER
               WHEN "U" 
                    PERFORM OPERACION-ACTUALIZAR
               WHEN "D" 
                    PERFORM OPERACION-ELIMINAR
               WHEN "Q" 
                    DISPLAY "Saliendo..."
               WHEN OTHER 
                    DISPLAY "Opción no válida."
           END-EVALUATE.
           
       
       OPERACION-CREAR.
           PERFORM CLEAR-SCREEN.
           DISPLAY "----- Crear usuario -----" LINE 3 POSITION 20.
           MOVE "CC" TO TIPDOC.
           DISPLAY "Ingrese Número de Documento:"  LINE 6  POSITION 20.
           ACCEPT NUMDOC                           LINE 6  POSITION 55.
           DISPLAY "Ingrese Nombre y Apellido:"    LINE 7  POSITION 20.
           ACCEPT NOMAPE                           LINE 7  POSITION 55.
           DISPLAY "Cargo (1=Empleado, 2=Admin):"  LINE 8  POSITION 20.
           ACCEPT CARGO                            LINE 8  POSITION 55.
           DISPLAY "Ingrese Monto a Pagar:"        LINE 9  POSITION 20.
           ACCEPT MONTPA                           LINE 9  POSITION 55.
           DISPLAY "Ingrese Detalles:"             LINE 10 POSITION 20.
           ACCEPT DETALL                           LINE 10 POSITION 55.
           DISPLAY "Ingrese Número de Contacto:"   LINE 11 POSITION 20.
           ACCEPT NUMCON                           LINE 11 POSITION 55.
           DISPLAY "Ingrese Correo:"               LINE 12 POSITION 20.
           ACCEPT CORREO                           LINE 12 POSITION 55.
           DISPLAY "Fecha de Registro (AAAAMMDD):" LINE 13 POSITION 20.
           ACCEPT FECREG                           LINE 13 POSITION 55.
           COMPUTE REDOND ROUNDED = (NUMDOC + NUMCON) / 2.
           MOVE REDOND TO CODUNI.
           DISPLAY CODUNI LINE 15 POSITION 30.
           PERFORM PAUSA.
           WRITE USUDATA.    
           IF WS-FS NOT = "00"
              DISPLAY "Error al crear usuario. FS = " WS-FS
           ELSE
              DISPLAY "Usuario creado exitosamente."
           END-IF.
           PERFORM PAUSA.
           
       
       OPERACION-LEER.
           PERFORM CLEAR-SCREEN.
           DISPLAY "----- Leer usuario -----" LINE 3 POSITION 20.
           DISPLAY "Ingrese Número de Documento:" LINE 5 POSITION 20.
           ACCEPT NUMDOC            LINE 5 POSITION 55.
           READ ARCHIVO-CLI
                RECORD KEY NUMDOC           
                INVALID KEY
                    DISPLAY "Usuario no encontrado." LINE 7 POSITION 20
                NOT INVALID KEY
            DISPLAY "------------------------------" LINE 7 POSITION 20
            DISPLAY "Tipo Doc    : " LINE 8  POSITION 20
            TIPDOC                   LINE 8  POSITION 40
            DISPLAY "Num Doc     : " LINE 9  POSITION 20
            NUMDOC                   LINE 9  POSITION 40
            DISPLAY "Nombre      : " LINE 10 POSITION 20
            NOMAPE                   LINE 10 POSITION 40
            DISPLAY "Cargo       : " LINE 11 POSITION 20
            CARGO                    LINE 11 POSITION 40
            DISPLAY "Monto Pagar : " LINE 12 POSITION 20
            MONTPA                   LINE 12 POSITION 40
            DISPLAY "Detalles    : " LINE 13 POSITION 20
            DETALL                   LINE 13 POSITION 40
            DISPLAY "Contacto    : " LINE 14 POSITION 20
            NUMCON                   LINE 14 POSITION 40
            DISPLAY "Correo      : " LINE 15 POSITION 20
            CORREO                   LINE 15 POSITION 40
            DISPLAY "Fecha Reg   : " LINE 16 POSITION 20
            FECREG                   LINE 16 POSITION 40
            DISPLAY "Cod Uni     : " LINE 17 POSITION 20
            CODUNI                   LINE 17 POSITION 40
           END-READ.
           PERFORM PAUSA.
           
       
       OPERACION-ACTUALIZAR.
           PERFORM CLEAR-SCREEN.
           DISPLAY "----- Actualizar usuario -----" LINE 3 POSITION 20.
           DISPLAY "Ingrese Número de Documento:" LINE 5 POSITION 20.
           ACCEPT NUMDOC  LINE 5 POSITION 55.
           READ ARCHIVO-CLI RECORD KEY NUMDOC
                INVALID KEY
                    DISPLAY "Usuario no encontrado." LINE 7 POSITION 20
                NOT INVALID KEY
            DISPLAY "Datos actuales:" LINE 8 POSITION 20
            DISPLAY "Nombre      : " NOMAPE  LINE 9 POSITION 20
            DISPLAY "Cargo       : " CARGO   LINE 10 POSITION 20
            DISPLAY "Monto Pagar : " MONTPA  LINE 11 POSITION 20
            DISPLAY "Detalles    : " DETALL  LINE 12 POSITION 20
            DISPLAY "Contacto    : " NUMCON  LINE 13 POSITION 20
            DISPLAY "Correo      : " CORREO  LINE 14 POSITION 20
            DISPLAY "Fecha Reg   : " FECREG  LINE 15 POSITION 20
            DISPLAY "nuevo Nombre y Apellido:" LINE 16 POSITION 20
            ACCEPT NOMAPE  LINE 16 POSITION 55
            DISPLAY "Ingrese nuevo Cargo:" LINE 17 POSITION 20
            ACCEPT CARGO   LINE 17 POSITION 55
            DISPLAY "Ingrese nuevo Monto a Pagar:" LINE 18 POSITION 20
            ACCEPT MONTPA  LINE 18 POSITION 55
            DISPLAY "Ingrese nuevos Detalles:" LINE 19 POSITION 20
            ACCEPT DETALL  LINE 19 POSITION 55
            DISPLAY "Nuevo Número de Contacto:" LINE 20 POSITION 20
            ACCEPT NUMCON  LINE 20 POSITION 55
            DISPLAY "Ingrese nuevo Correo:" LINE 21 POSITION 20
            ACCEPT CORREO  LINE 21 POSITION 55
            DISPLAY "Nueva Fecha de Registro:" LINE 22 POSITION 20
            ACCEPT FECREG  LINE 22 POSITION 55
            COMPUTE REDOND ROUNDED = (NUMDOC + NUMCON) / 2
            MOVE REDOND TO CODUNI
            REWRITE USUDATA
            IF WS-FS NOT = "00"
                DISPLAY "Error al actualizar usuario. FS = " WS-FS
            ELSE
                DISPLAY "Usuario actualizado exitosamente."
            END-IF
           END-READ.
           PERFORM PAUSA.
           
       
       OPERACION-ELIMINAR.
           PERFORM CLEAR-SCREEN.
           DISPLAY "----- Eliminar usuario -----" LINE 3 POSITION 20.
           DISPLAY "Ingrese Número de Documento:" LINE 5 POSITION 20.
           ACCEPT NUMDOC  LINE 5 POSITION 55.
           READ ARCHIVO-CLI RECORD KEY NUMDOC
                INVALID KEY
                    DISPLAY "Usuario no encontrado." LINE 7 POSITION 20
                NOT INVALID KEY
                    DELETE ARCHIVO-CLI
                    IF WS-FS NOT = "00"
                        DISPLAY "Error al eliminar usuario. FS = " WS-FS
                    ELSE
                        DISPLAY "Usuario eliminado exitosamente."
                    END-IF
           END-READ.
           PERFORM PAUSA.
           
       
       CLEAR-SCREEN.
           MOVE " " TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.
       
       PAUSA.
           DISPLAY "Presione ENTER para continuar...".
           ACCEPT WS-OPCION.
