       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRASPASO-INFO.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-BASE ASSIGN TO "placli.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARCHIVO-SALIDA ASSIGN TO "data-user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS NUMDOC
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-BASE.
       01 REGISTRO PIC X(291).

       FD ARCHIVO-SALIDA.
       01 USUDATA.
           05 NUMDOC     PIC 9(14).
           05 RAZSOC     PIC X(51).
           05 DIRECT     PIC X(51).
           05 CIUDAD     PIC X(21).
           05 CONTAC     PIC X(31).
           05 TEL-1      PIC X(11).
           05 TEL-2      PIC X(11).
           05 CORREO     PIC X(101).

       WORKING-STORAGE SECTION.
       01 WS-DATA.
           05 WS-FS      PIC XX.
           05 WS-OPTI    PIC X VALUE SPACE.
           05 CONTADOR   PIC 9(10) VALUE 0.

       01 HEADER        PIC X(291) VALUE SPACES.

       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO CONTADOR.
           OPEN INPUT ARCHIVO-BASE.
           IF WS-FS NOT = "00"
               DISPLAY "ERROR ABRIENDO ARCHIVO BASE: " WS-FS
               STOP RUN
           END-IF.

           OPEN I-O ARCHIVO-SALIDA.
           IF WS-FS NOT = "00" AND WS-FS NOT = "05"
               DISPLAY "ERROR ABRIENDO ARCHIVO SALIDA: " WS-FS
               CLOSE ARCHIVO-BASE
               STOP RUN
           END-IF.

           READ ARCHIVO-BASE INTO HEADER.
           DISPLAY "PROCESANDO DATOS..." LINE 2 POSITION 10.

           PERFORM UNTIL WS-OPTI = "Q"
               READ ARCHIVO-BASE INTO REGISTRO
                   AT END MOVE "Q" TO WS-OPTI
                   NOT AT END PERFORM PROCESAR-REGISTRO
               END-READ
           END-PERFORM.

           DISPLAY "TOTAL REGISTROS PROCESADOS: " CONTADOR 
           LINE 10 POSITION 10.
           DISPLAY "PROCESO FINALIZADO. PRESIONE ENTER PARA SALIR." 
           LINE 12 POSITION 10.
           ACCEPT WS-OPTI.

           CLOSE ARCHIVO-BASE.
           CLOSE ARCHIVO-SALIDA.
           STOP RUN.

       PROCESAR-REGISTRO.
           UNSTRING REGISTRO DELIMITED BY ';'
               INTO NUMDOC, RAZSOC, DIRECT, 
               CIUDAD, CONTAC, TEL-1, TEL-2, CORREO.

           IF NUMDOC IS NUMERIC
               WRITE USUDATA
                INVALID KEY 
                   DISPLAY "ERROR: CLAVE DUPLICADA O INVALIDA: " NUMDOC
               NOT INVALID KEY 
                 ADD 1 TO CONTADOR
                 DISPLAY "USUARIO CREADO: " NUMDOC LINE 6 POSITION 10
               END-WRITE
           ELSE
               DISPLAY "ERROR EN REGISTRO: NUMDOC INVALIDO." 
               LINE 7 POSITION 10
           END-IF.
