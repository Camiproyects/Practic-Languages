       IDENTIFICATION DIVISION.
       PROGRAM-ID. INICIO_SESION.
       AUTHOR. "ANDRES CAMILO LAGUNA BERNAL".
       DATE-WRITTEN. "03-01-2025".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLIENTES ASSIGN TO "manage/usuarios.py"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-CLIENTES.
       01 USUDATA.
           05 CODUNI   PIC 9(10).
           05 NOMAPE   PIC X(65).

       WORKING-STORAGE SECTION.
       01 VARIABLES.
           03 DATAVARIABLE.
              05 TIPDOC   PIC X(02).
              05 NUMDOC   PIC 9(10).
              05 MONTPA   PIC 9(10).
              05 NUMCON   PIC 9(10).
              05 CORREO   PIC X(30).
              05 CARGO    PIC X.
              05 DETALL   PIC X(65).
              05 FECREG   PIC 9(08).
       01 PANTALLA.
           03 MANAGE.
              05 LIM      PIC X.
              05 DAT      PIC X.
              05 MAR      PIC X(60).
              05 SW-KILL  PIC X.
       01 PERSIS.
           05 SW-NOMAPE   PIC X(65).
           05 SW-TIPDOC   PIC X(02).
           05 SW-NUMDOC   PIC 9(10).
           05 SW-MONTPA   PIC 9(10).
           05 SW-NUMCON   PIC 9(10).
           05 SW-CORREO   PIC X(30).
           05 SW-CARGO    PIC X.
           05 SW-DETALL   PIC X(65).
           05 SW-CODUNI   PIC 9(10).
           05 SW-FECREG   PIC 9(08).
       01 WS-OPCION      PIC X.
       01 WS-DATO-JSON   PIC X(500).

       PROCEDURE DIVISION.
       INICIO.
           MOVE ' ' TO LIM.
           MOVE ALL '*' TO MAR.
           DISPLAY LIM ERASE EOS.
           PERFORM MENU-CRUD UNTIL WS-OPCION = 'Q'.
           DISPLAY "PROGRAMA TERMINADO".
           STOP RUN.

       MENU-CRUD.
           DISPLAY "Seleccione operación:"     LINE 5  POSITION 20.
           DISPLAY "  C -> Crear usuario"      LINE 9  POSITION 20.
           DISPLAY "  R -> Leer usuarios"      LINE 11 POSITION 20.
           DISPLAY "  U -> Actualizar usuario" LINE 13 POSITION 20.
           DISPLAY "  D -> Eliminar usuario"   LINE 15 POSITION 20.
           DISPLAY "  Q -> Salir"              LINE 17 POSITION 20.
           ACCEPT WS-OPCION.
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
           DISPLAY LIM ERASE EOS.
           DISPLAY "----- Crear usuario -----" LINE 5 POSITION 20.
           DISPLAY "Ingrese Tipo Numero Identidad:" LINE 6 POSITION 20.
           MOVE 'CC' TO TIPDOC.
           ACCEPT NUMDOC LINE 6 POSITION 55.
           DISPLAY "Ingrese Nombre y Apellido:" LINE 7 POSITION 20.
           ACCEPT NOMAPE LINE 7 POSITION 55.
           DISPLAY "1=Empleado 2=Admin:" LINE 8 POSITION 20.
           ACCEPT CARGO LINE 8 POSITION 55.
           DISPLAY "Ingrese Monto a Pagar:" LINE 9 POSITION 20.
           ACCEPT MONTPA LINE 9 POSITION 55.
           DISPLAY "Ingrese Detalles:" LINE 10 POSITION 20.
           ACCEPT DETALL LINE 10 POSITION 55.
           DISPLAY "Ingrese Número de Contacto:" LINE 11 POSITION 20.
           ACCEPT NUMCON LINE 11 POSITION 55.
           DISPLAY "Ingrese Correo:" LINE 12 POSITION 20.
           ACCEPT CORREO LINE 12 POSITION 55.
           DISPLAY "Ingrese Fecha de Registro :" LINE 13 POSITION 20.
           ACCEPT FECREG LINE 13 POSITION 55.
           COMPUTE CODUNI = (NUMDOC + NUMCON) / 2.
           STRING 
              '{"operacion": "C", "datos": {' DELIMITED BY SIZE
              '"tipdoc": "' TIPDOC '", ' DELIMITED BY SIZE
              '"numdoc": ' NUMDOC ', ' DELIMITED BY SIZE
              '"nomape": "' NOMAPE '", ' DELIMITED BY SIZE
              '"cargo": "' CARGO '", ' DELIMITED BY SIZE
              '"montpa": ' MONTPA ', ' DELIMITED BY SIZE
              '"detall": "' DETALL '", ' DELIMITED BY SIZE
              '"numcon": ' NUMCON ', ' DELIMITED BY SIZE
              '"correo": "' CORREO '", ' DELIMITED BY SIZE
              '"fecreg": ' FECREG ', ' DELIMITED BY SIZE
              '"coduni": ' CODUNI '}}' DELIMITED BY SIZE
           INTO WS-DATO-JSON
           END-STRING.
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de creación enviada.".
           
       OPERACION-LEER.
           DISPLAY "----- Leer usuarios -----".
           STRING '{"operacion": "R"}' DELIMITED BY SIZE
           INTO WS-DATO-JSON
           END-STRING.
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de lectura enviada.".
           
       OPERACION-ACTUALIZAR.           DISPLAY LIM ERASE EOS.
           DISPLAY "----- Crear usuario -----" LINE 5 POSITION 20.
           DISPLAY "Ingrese Tipo Numero Identidad:" LINE 6 POSITION 20.
           ACCEPT TIPDOC LINE 6 POSITION 55.
           ACCEPT NUMDOC LINE 6 POSITION 55.
           DISPLAY "Ingrese Nombre y Apellido:" LINE 7 POSITION 20.
           ACCEPT NOMAPE LINE 7 POSITION 55.
           DISPLAY "1=Empleado 2=Admin:" LINE 8 POSITION 20.
           ACCEPT CARGO LINE 8 POSITION 55.
           DISPLAY "Ingrese Monto a Pagar:" LINE 9 POSITION 20.
           ACCEPT MONTPA LINE 9 POSITION 55.
           DISPLAY "Ingrese Detalles:" LINE 10 POSITION 20.
           ACCEPT DETALL LINE 10 POSITION 55.
           DISPLAY "Ingrese Número de Contacto:" LINE 11 POSITION 20.
           ACCEPT NUMCON LINE 11 POSITION 55.
           DISPLAY "Ingrese Correo:" LINE 12 POSITION 20.
           ACCEPT CORREO LINE 12 POSITION 55.
           DISPLAY "Ingrese Fecha de Registro :" LINE 13 POSITION 20.
           ACCEPT FECREG LINE 13 POSITION 55.
           COMPUTE CODUNI = (NUMDOC + NUMCON) / 2.
           STRING 
              '{"operacion": "U", "datos": {' DELIMITED BY SIZE
              '"tipdoc": "' TIPDOC '", ' DELIMITED BY SIZE
              '"numdoc": ' NUMDOC ', ' DELIMITED BY SIZE
              '"nomape": "' NOMAPE '", ' DELIMITED BY SIZE
              '"cargo": "' CARGO '", ' DELIMITED BY SIZE
              '"montpa": ' MONTPA ', ' DELIMITED BY SIZE
              '"detall": "' DETALL '", ' DELIMITED BY SIZE
              '"numcon": ' NUMCON ', ' DELIMITED BY SIZE
              '"correo": "' CORREO '", ' DELIMITED BY SIZE
              '"fecreg": ' FECREG ', ' DELIMITED BY SIZE
              '"coduni": ' CODUNI '}}' DELIMITED BY SIZE
           INTO WS-DATO-JSON
           END-STRING.
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de actualización enviada.".
           

       OPERACION-ELIMINAR.
           DISPLAY "----- Eliminar usuario -----".
           DISPLAY "Ingrese Cédula de Ciudadania :".
           ACCEPT TIPDOC.
           ACCEPT NUMDOC.
           STRING 
              '{"operacion": "D", "datos": {' DELIMITED BY SIZE
              '"tipdoc": "' TIPDOC '", ' DELIMITED BY SIZE
              '"numdoc": ' NUMDOC '}}' DELIMITED BY SIZE
           INTO WS-DATO-JSON
           END-STRING.
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de eliminación enviada.".
