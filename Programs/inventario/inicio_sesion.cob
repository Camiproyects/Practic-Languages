       IDENTIFICATION DIVISION.
       PROGRAM-ID. inicio_sesion.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
       DATE-WRITTEN.03-01-2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLIENTES ASSIGN TO "manage/usuarios.py"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-CLIENTES.
       01 USUDATA.
           05 CODUNI          PIC 9(10).
           05 NOMAPE          PIC X(65).
           
       WORKING-STORAGE SECTION.
       01  VARIABLES.
           03 DATAVARIABLE.
              05 TIPDOC       PIC X(02).
              05 NUMDOC       PIC 9(10).
              05 MONTPA       PIC 9(10).
              05 NUMCON       PIC 9(10).
              05 CORREO       PIC X(30).
              05 CARGO        PIC X.
              05 DETALL       PIC X(65).
              05 FECREG       PIC 9(08).
       01  PANTALLA.
           03 MANAGE.
              05 LIM          PIC X.
              05 DAT          PIC X.
              05 MAR          PIC X(60).
              05 SW-KILL      PIC X.
       01 PERSIS.
              05 SW-NOMAPE       PIC X(65).
              05 SW-TIPDOC       PIC X(02).
              05 SW-NUMDOC       PIC 9(10).
              05 SW-MONTPA       PIC 9(10).
              05 SW-NUMCON       PIC 9(10).
              05 SW-CORREO       PIC X(30).
              05 SW-CARGO        PIC X.
              05 SW-DETALL       PIC X(65).
              05 SW-CODUNI       PIC 9(10).
              05 SW-FECREG       PIC 9(08).

       PROCEDURE DIVISION.
       INICIO.
           MOVE ' ' TO LIM.
           MOVE ALL '*' TO MAR.
           DISPLAY LIM ERASE EOS.
           DISPLAY 'CEDULA DE CIUDADANIA      :' LINE 11 POSITION 20.
           DISPLAY 'NOMBRE APELLIDO           :' LINE 12 POSITION 20.
           DISPLAY 'CARGO 1(EMPLEADO) 2(ADMIN):' LINE 13 POSITION 20.
           DISPLAY 'MONTO A PAGAR             :' LINE 14 POSITION 20.
           DISPLAY 'DETALLES                  :' LINE 15 POSITION 20.
           DISPLAY 'NUMERO DE CONTACTO        :' LINE 16 POSITION 20.
           DISPLAY 'CORREO                    :' LINE 17 POSITION 20.
           PERFORM TOMA-DATOS UNTIL SW-KILL = 'Q'.

       TOMA-DATOS.
           ACCEPT NUMDOC             LINE 11  POSITION 65.
           ACCEPT NOMAPE             LINE 12  POSITION 65.
           ACCEPT CARGO              LINE 13  POSITION 65.
           ACCEPT MONTPA             LINE 14  POSITION 65.
           ACCEPT DETALL             LINE 15  POSITION 65.
           ACCEPT NUMCON             LINE 16  POSITION 65. 
           ACCEPT CORREO             LINE 17  POSITION 65.
           COMPUTE CODUNI = (NUMCON + NUMDOC) / 2.
           PERFORM VALID-DATA UNTIL SW-KILL = 'E'.
      
       VALID-DATA.
           OPEN I-O ARCHIVO-CLIENTES
           MOVE NUMDOC               TO SW-NUMDOC
           MOVE NOMAPE               TO SW-NOMAPE
           MOVE CARGO                TO SW-CARGO
           MOVE MONTPA               TO SW-MONTPA
           MOVE DETALL               TO SW-DETALL
           MOVE NUMCON               TO SW-NUMCON
           MOVE CORREO               TO SW-CORREO
           MOVE CODUNI               TO SW-CODUNI
           MOVE TIPDOC               TO SW-TIPDOC
           MOVE FECREG               TO SW-FECREG
           PERFORM READ-FILE
           CLOSE ARCHIVO-CLIENTES
           MOVE 'E' TO SW-KILL.

       READ-FILE.
           READ ARCHIVO-CLIENTES INTO USUDATA
               AT END
                   DISPLAY 'FIN DEL ARCHIVO'
                   MOVE 'Q' TO SW-KILL
           END-READ.

       FINISH.
           DISPLAY 'PROGRAMA TERMINADO'.
           STOP RUN.
