       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRASPASO-INFO.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-BASE ASSIGN TO "placli.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

           SELECT ARCHIVO-SALIDA ASSIGN TO "data-user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS NUMDOC
               FILE STATUS IS WS-FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-BASE.
       01 REGISTRO PIC X(291).

       FD ARCHIVO-SALIDA.
       01 USUDATA.
           05 NUMDOC     PIC 9(13).
           05 RAZSOC     PIC X(50).
           05 DIRECT     PIC X(50).
           05 CIUDAD     PIC X(20).
           05 CONTAC     PIC X(30).
           05 TEL-1      PIC X(10).
           05 TEL-2      PIC X(10).
           05 CORREO     PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-DATA.
           05 WS-FS          PIC XX.
           05 WS-FS-SALIDA   PIC XX.
           05 WS-OPTI        PIC X     VALUE SPACE.
           05 CONTADOR       PIC 9(10) VALUE 0.
           05 LIN            PIC 9(5)  VALUE 0.

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
           IF WS-FS-SALIDA NOT = "00" AND WS-FS-SALIDA NOT = "05"
               DISPLAY "ERROR ABRIENDO ARCHIVO SALIDA: " WS-FS-SALIDA
               CLOSE ARCHIVO-BASE
               STOP RUN
           END-IF.

           READ ARCHIVO-BASE INTO HEADER
               AT END 
                   DISPLAY "Archivo vacío o sin encabezado."
                   CLOSE ARCHIVO-BASE
                   CLOSE ARCHIVO-SALIDA
                   STOP RUN.

           DISPLAY "PROCESANDO DATOS..." LINE 2 POSITION 10.

           MOVE 10 TO LIN.

           PERFORM UNTIL WS-OPTI = "Q"
               READ ARCHIVO-BASE INTO REGISTRO
                   AT END MOVE "Q" TO WS-OPTI
                   NOT AT END 
                   ADD 1 TO CONTADOR
                   PERFORM PROCESAR-REGISTRO
                   PERFORM VER
               END-READ
           END-PERFORM.

           CLOSE ARCHIVO-BASE.
           CLOSE ARCHIVO-SALIDA.
           STOP RUN.
       
       PROCESAR-REGISTRO.
           UNSTRING REGISTRO DELIMITED BY ';'
               INTO NUMDOC, RAZSOC, DIRECT, 
               CIUDAD, CONTAC, TEL-1, TEL-2, CORREO.
           
           WRITE USUDATA.
           
       VER.
           ADD 1 TO LIN.
           DISPLAY CONTADOR LINE 2 POSITION 50.
           DISPLAY "*"     LINE LIN POSITION 1.
           DISPLAY "*"     LINE LIN POSITION 12.
           DISPLAY "*"     LINE LIN POSITION 34.
           DISPLAY "*"     LINE LIN POSITION 45.
           DISPLAY "*"     LINE LIN POSITION 56.
           DISPLAY NUMDOC  LINE LIN POSITION 2.
           DISPLAY RAZSOC  LINE LIN POSITION 13.
           DISPLAY CIUDAD  LINE LIN POSITION 35.
           DISPLAY CONTAC  LINE LIN POSITION 46.
           DISPLAY TEL-1   LINE LIN POSITION 57.
           IF LIN > 20 
              MOVE 10 TO LIN
           END-IF.
