      *>------------------------------------------------------------
      *> Programa: INICIO_SESION
      *> Autor: ANDRES CAMILO LAGUNA BERNAL
      *> Fecha de creación: 03-01-2025
      *>------------------------------------------------------------
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
      *> Se prepara la pantalla y se muestra un encabezado
           MOVE ' ' TO LIM.
           MOVE ALL '*' TO MAR.
           DISPLAY LIM ERASE EOS.
      *> Se ejecuta el menú del CRUD hasta que el usuario decida salir ('Q')
           PERFORM MENU-CRUD UNTIL WS-OPCION = 'Q'.
           DISPLAY "PROGRAMA TERMINADO".
           STOP RUN.

       MENU-CRUD.
      *> Presenta el menú de opciones al usuario
           DISPLAY "Seleccione operación:".
           DISPLAY "  C -> Crear usuario".
           DISPLAY "  R -> Leer usuarios".
           DISPLAY "  U -> Actualizar usuario".
           DISPLAY "  D -> Eliminar usuario".
           DISPLAY "  Q -> Salir".
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
      *> Solicita los datos para crear un usuario
           DISPLAY "----- Crear usuario -----".
           DISPLAY "Ingrese Cédula de Ciudadania (TIPDOC y NUMDOC):".
           ACCEPT TIPDOC.
           ACCEPT NUMDOC.
           DISPLAY "Ingrese Nombre y Apellido:".
           ACCEPT NOMAPE.
           DISPLAY "Ingrese Cargo (1=Empleado, 2=Admin):".
           ACCEPT CARGO.
           DISPLAY "Ingrese Monto a Pagar:".
           ACCEPT MONTPA.
           DISPLAY "Ingrese Detalles:".
           ACCEPT DETALL.
           DISPLAY "Ingrese Número de Contacto:".
           ACCEPT NUMCON.
           DISPLAY "Ingrese Correo:".
           ACCEPT CORREO.
           DISPLAY "Ingrese Fecha de Registro (AAAAMMDD):".
           ACCEPT FECREG.
      *> Calcula el código único como promedio de NUMDOC y NUMCON
           COMPUTE CODUNI = (NUMDOC + NUMCON) / 2.
      *> Construye la cadena JSON con la operación y los datos
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
      *> Se abre el archivo en modo EXTEND para agregar la operación sin borrar las existentes
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de creación enviada.".
           

       OPERACION-LEER.
      *> Envía la operación de lectura
           DISPLAY "----- Leer usuarios -----".
           STRING '{"operacion": "R"}' DELIMITED BY SIZE
           INTO WS-DATO-JSON
           END-STRING.
           OPEN EXTEND ARCHIVO-CLIENTES.
           WRITE USUDATA FROM WS-DATO-JSON.
           CLOSE ARCHIVO-CLIENTES.
           DISPLAY "Operación de lectura enviada.".
           

       OPERACION-ACTUALIZAR.
      *> Solicita los datos para actualizar un usuario
           DISPLAY "----- Actualizar usuario -----".
           DISPLAY "Ingrese Cédula de Ciudadania (TIPDOC y NUMDOC)".
           ACCEPT TIPDOC.
           ACCEPT NUMDOC.
           DISPLAY "Ingrese nuevo Nombre y Apellido:".
           ACCEPT NOMAPE.
           DISPLAY "Ingrese nuevo Cargo (1=Empleado, 2=Admin):".
           ACCEPT CARGO.
           DISPLAY "Ingrese nuevo Monto a Pagar:".
           ACCEPT MONTPA.
           DISPLAY "Ingrese nuevos Detalles:".
           ACCEPT DETALL.
           DISPLAY "Ingrese nuevo Número de Contacto:".
           ACCEPT NUMCON.
           DISPLAY "Ingrese nuevo Correo:".
           ACCEPT CORREO.
           DISPLAY "Ingrese nueva Fecha de Registro (AAAAMMDD):".
           ACCEPT FECREG.
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
      *> Solicita la identificación del usuario a eliminar
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
